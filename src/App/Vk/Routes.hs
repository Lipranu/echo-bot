{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Vk.Routes
  ( getLongPollServer
  , getUpdates
  , handlers
  ) where

-- IMPORTS -----------------------------------------------------------------

import App.Shared.Config
import App.Shared.Routes ( MonadEffects, MonadRepetitions )
import App.Vk.Config     ( VkReader )
import App.Vk.Converters
import App.Vk.Requests
import App.Vk.Responses

import Infrastructure.Has
import Infrastructure.Logger    hiding ( Priority (..) )
import Infrastructure.Requester

import qualified App.Shared.Routes as S

import Control.Monad          ( (>=>), replicateM_, when )
import Control.Monad.Catch    ( Handler (..), MonadThrow, MonadCatch )
import Control.Monad.IO.Class ( MonadIO (..) )
import Control.Monad.State    ( MonadState, evalStateT, runStateT, modify )
import Data.Aeson             ( FromJSON )
import Data.Maybe             ( isJust, fromMaybe, listToMaybe )
import Data.Text.Extended     ( Text, showt )
import System.Random          ( randomIO )

import qualified Data.Text.Extended as Text

-- TYPES -------------------------------------------------------------------

type AppReader r m = (SharedReader r m, VkReader r m)

-- FUNCTIONS ---------------------------------------------------------------

getLongPollServer
  :: (VkReader r m, MonadEffects r m, MonadThrow m)
  => m GetUpdates
getLongPollServer = mkGetUpdates <$> fromResponseR GetLongPollServer

getUpdates
  :: ( AppReader env m
     , MonadCatch m
     , MonadEffects env m
     , MonadRepetitions env m
     , MonadThrow m
     )
  => GetUpdates
  -> m ()
getUpdates gu = S.withLog requestAndDecode gu >>= \case
  Updates xs ts -> do
    result <- S.fromValues xs
    traverseHandled_ processMessage result
    getUpdates gu { guTs = ts }
  OutOfDate ts  -> getUpdates gu { guTs = showt ts }
  rest          -> getLongPollServer >>= getUpdates

processMessage
  :: ( AppReader env m
     , MonadCatch m
     , MonadEffects env m
     , MonadRepetitions env m
     )
  => Update
  -> m ()
processMessage (NewMessage m) = do
  continue <- evalStateT (processCommand $ getter m) m
  when continue $ do
    (attach, msg)  <- runStateT (stateProcess $ getter m) m
    let sendable = check msg attach
    report sendable
    when sendable $ do
      (kb, repeats)  <- findUser $ getter m
      let sm = mkSendMessage msg attach kb
      replicateM_ repeats $ sendMessage sm
  where
    stateProcess = S.fromValues >=> traverseHandled processAttachment

    findUser key = S.getRepeats key >>= \case
      Just i  -> pure (Nothing, i)
      Nothing -> do
        repeat <- unDefaultRepeat <$> obtain
        S.putRepeats repeat key
        pure (mkKeyboard, repeat)

    check Message {..} attach = maybe False (not . Text.null) mMessage
      || (isJust mLatitude && isJust mLongitude)
      || isJust mSticker
      || not (null attach)

    report True  = logDebug   ("Message can be sended"   :: Text)
    report False = logWarning ("Cant send empty message" :: Text)

processMessage rest = pure ()

processCommand
  :: ( MonadEffects env m
     , MonadRepetitions env m
     , MonadState Message m
     , AppReader env m
     , MonadCatch m
     )
  => Maybe Command
  -> m Bool
processCommand Nothing    = pure True
processCommand (Just cmd) = logData cmd >> case cmd of
  UnknownCommand _ -> pure True
  Help             -> do
    text <- unHelpText <$> obtain
    reply text
    pure False
  NewCount i       -> do
    S.putRepeats i =<< grab
    reply $ "Repeat count set to: " <> showt i
    pure False
  Repeat           -> do
    text    <- unRepeatText    <$> obtain
    def     <- unDefaultRepeat <$> obtain
    current <- S.getRepeats    =<< grab
    let repeats = fromMaybe def current
    reply $ text <> "\nCurrent repeat count: " <> showt repeats
    pure False
  where reply text = getName >>= mkCommandReply text >>= sendMessage

getName
  :: ( MonadEffects r m
     , VkReader r m
     , MonadState Message m
     , MonadThrow m
     , MonadCatch m
     )
  => m (Maybe UserName)
getName = grab >>= \case
  Private -> pure Nothing
  Chat    -> do
    response <- mkGetName >>= fromResponseH
    pure $ response >>= listToMaybe

sendMessage
  :: (MonadEffects r m, MonadIO m, VkReader r m, MonadThrow m)
  => (Int -> SendMessage)
  -> m ()
sendMessage sm = do
  msg <- sm <$> liftIO randomIO
  fromResponse_ @MessageSended msg

processAttachment
  :: ( MonadCatch m
     , MonadEffects env m
     , MonadIO m
     , MonadState Message m
     , VkReader env m
     )
  => Attachment
  -> m (Maybe Text)
processAttachment Graffiti     = notifyCantSend "graffiti" >> pure Nothing
processAttachment (Sticker id) = addSticker id     >> pure Nothing
processAttachment (Attachment   body) = addAttachment body
processAttachment (AudioMessage body) = uploadAttachment body
processAttachment (Photo        body) = grab >>= \case
  Private -> addAttachment body
  Chat    -> uploadAttachment body
processAttachment (Video        body) = grab >>= \case
  Private -> addAttachment body
  Chat | vbCanResend body -> addAttachment body
       | otherwise        -> notifyCantSend "uploaded video" >> pure Nothing
processAttachment (Document     body) = case dbType body of
  "graffiti" -> notifyCantSend "graffiti" >> pure Nothing
  "photo"    -> uploadAttachment $ docToPhoto body
  _          -> uploadAttachment body

uploadAttachment
  :: ( FromJSON output
     , Loggable output
     , MonadEffects env m
     , MonadIO m
     , MonadThrow m
     , ToAttachment output
     , ToUploadRequests m input output
     , VkReader env m
     )
  => input
  -> m (Maybe Text)
uploadAttachment = toUploadRequests >=> processDocument

notifyCantSend
  :: ( MonadCatch m
     , MonadEffects env m
     , MonadIO m
     , MonadState Message m
     , MonadThrow m
     , VkReader env m
     )
  => Text
  -> m ()
notifyCantSend aType = getName >>= mkNotification aType >>= sendMessage

addAttachment
  :: forall input m
   . (Applicative m, ToAttachment input)
  => input
  -> m (Maybe Text)
addAttachment = pure . Just . toAttachment @input

addSticker :: MonadState Message m => Integer -> m ()
addSticker id = modify $ \m -> m { mSticker = Just id }

processDocument
  :: forall result s env m
   . ( FromJSON result
     , Loggable result
     , MonadEffects env m
     , MonadIO m
     , MonadThrow m
     , ToAttachment result
     , VkReader env m
     )
  => UploadRequests result
  -> m (Maybe Text)
processDocument UploadRequests {..} = do
  server   <- fromResponseR getUploadServer
  file     <- S.inputLog request getFile
  uploaded <- fromResponseU $ uploadFile server file
  saved    <- fromResponseR $ saveFile uploaded
  addAttachment @result saved

fromResponseR, fromResponseU
  :: forall output input env m
   . ( FromJSON output
     , Loggable input
     , Loggable output
     , MonadEffects env m
     , MonadThrow m
     , ToRequest m input
     )
  => input
  -> m output
fromResponseR = S.fromResponse @ResponseException @output
fromResponseU = S.fromResponse @UploadException   @output

fromResponse_
  :: forall output input env m
   . ( FromJSON output
     , Loggable input
     , Loggable output
     , MonadEffects env m
     , MonadThrow m
     , ToRequest m input
     )
  => input
  -> m ()
fromResponse_ = S.fromResponse_ @ResponseException @output

fromResponseH
  :: forall output input env m
   . ( FromJSON output
     , Loggable input
     , Loggable output
     , MonadCatch m
     , MonadEffects env m
     , MonadThrow m
     , ToRequest m input
     )
  => input
  -> m (Maybe output)
fromResponseH = S.fromResponseH @ResponseException @output handlers

traverseHandled
  :: (MonadCatch m, HasLogger env m, Loggable input)
  => (input -> m (Maybe output))
  -> [input]
  -> m [output]
traverseHandled = S.traverseHandled handlers

traverseHandled_
  :: (MonadCatch m, HasLogger env m, Loggable input)
  => (input -> m ())
  -> [input]
  -> m ()
traverseHandled_ = S.traverseHandled_ handlers

handlers :: HasLogger env m => output -> [Handler m output]
handlers x = S.handlers x <>
  [ Handler $ \(e :: ResponseException) -> logData e >> pure x
  , Handler $ \(e :: UploadException)   -> logData e >> pure x
  ]
