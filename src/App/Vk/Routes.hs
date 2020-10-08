{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE RecordWildCards     #-}

module App.Vk.Routes
  ( getLongPollServer
  , getUpdates
  , handlers
  ) where

-- IMPORTS -----------------------------------------------------------------

import App.Shared.Config
import App.Shared.Routes hiding ( fromValues
                                , fromValues_
                                , fromResponse
                                , fromResponse_
                                , fromResponseH
                                , handlers
                                )

import App.Vk.Config     ( VkReader )
import App.Vk.Converters
import App.Vk.Requests
import App.Vk.Responses

import Infrastructure.Has
import Infrastructure.Logger    hiding ( Priority (..) )
import Infrastructure.Requester

import qualified App.Shared.Routes as Shared

import Control.Monad          ( (>=>), replicateM_, when )
import Control.Monad.Catch    ( Handler (..), MonadThrow, MonadCatch )
import Control.Monad.IO.Class ( MonadIO (..) )
import Control.Monad.State    ( MonadState, evalStateT, runStateT, modify )
import Data.Aeson             ( FromJSON, Value )
import Data.Maybe             ( catMaybes, isJust, fromMaybe, listToMaybe )
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
getUpdates gu = withLog requestAndDecode gu >>= \case
  Updates xs ts -> do
    fromValues_ processMessage xs
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
    result         <- runStateT (fromValues processAttachment $ getter m) m
    (kb, repeats)  <- findUser $ getter m
    (sm, sendable) <- formAndCheck result kb
    when sendable $ replicateM_ repeats $ sendMessage sm
  where
    findUser x = getRepeats x >>= \case
      Nothing -> (mkKeyboard,) . unDefaultRepeat <$> obtain
      Just i  -> pure (Nothing, i)

    formAndCheck (attach, msg) kb = (mkSendMessage msg attach kb,) <$>
      case checkMessage msg || checkAttachments attach of
        True  -> logDebug   ("Message can be sended"   :: Text) >> pure True
        False -> logWarning ("Cant send empty message" :: Text) >> pure False

    checkAttachments = not . null . catMaybes

    checkMessage Message {..} = isJust mSticker
      || maybe False (not . Text.null) mMessage
      || (isJust mLatitude && isJust mLongitude)

processMessage rest = pure mempty

processCommand
  :: ( MonadEffects env m
     , MonadRepetitions env m
     , MonadState s m
     , AppReader env m
     , Has Key s
     , Has PeerId s
     , Has FromId s
     , Has Context s
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
    putRepeats i =<< grab
    reply $ "Repeat count set to: " <> showt i
    pure False
  Repeat           -> do
    text    <- unRepeatText    <$> obtain
    def     <- unDefaultRepeat <$> obtain
    current <- getRepeats      =<< grab
    let repeats = fromMaybe def current
    reply $ text <> "\nCurrent repeat count: " <> showt repeats
    pure False
  where reply text = getName >>= mkCommandReply text >>= sendMessage

getName
  :: ( Has FromId s
     , Has Context s
     , MonadEffects r m
     , VkReader r m
     , MonadState s m
     , MonadThrow m
     , MonadCatch m
     )
  => m (Maybe UserName)
getName = grab >>= \case
  Private -> pure Nothing
  Chat    -> mkGetName >>= fmap listToMaybe . fromResponseH

sendMessage :: (MonadEffects r m, MonadIO m, VkReader r m, MonadThrow m)
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
processAttachment Graffiti     = notify "graffiti" >> pure Nothing
processAttachment (Sticker id) = addSticker id     >> pure Nothing
processAttachment (Attachment   body) = addAttachment body
processAttachment (AudioMessage body) = uploadAttachment body
processAttachment (Photo        body) = grab >>= \case
  Private -> addAttachment body
  Chat    -> uploadAttachment body
processAttachment (Video        body) = grab >>= \case
  Private -> addAttachment body
  Chat    -> case vbCanResend body of
    True  -> addAttachment body
    False -> notify "uploaded video" >> pure Nothing
processAttachment (Document     body) = case dbType body of
  "graffiti" -> notify "graffiti" >> pure Nothing
  "photo"    -> uploadAttachment $ docToPhoto body
  _          -> uploadAttachment body

uploadAttachment
  :: ( FromJSON output
     , HasPriority output
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

notify
  :: ( Has Context s
     , Has FromId s
     , Has MessageId s
     , Has PeerId s
     , MonadCatch m
     , MonadEffects env m
     , MonadIO m
     , MonadState s m
     , MonadThrow m
     , VkReader env m
     )
  => Text
  -> m ()
notify aType = getName >>= mkNotification aType >>= sendMessage

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
     , HasPriority result
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
  file     <- inputLog request getFile
  uploaded <- fromResponseU $ uploadFile server file
  saved    <- fromResponseR $ saveFile uploaded
  addAttachment @result saved

fromResponseR, fromResponseU
  :: forall output input env m
   . ( FromJSON output
     , HasPriority input
     , HasPriority output
     , MonadEffects env m
     , MonadThrow m
     , ToRequest m input
     )
  => input
  -> m output
fromResponseR = Shared.fromResponse @ResponseException @output
fromResponseU = Shared.fromResponse @UploadException   @output

fromResponse_
  :: forall output input env m
   . ( FromJSON output
     , HasPriority input
     , HasPriority output
     , MonadEffects env m
     , MonadThrow m
     , ToRequest m input
     )
  => input
  -> m ()
fromResponse_ = Shared.fromResponse_ @ResponseException @output

fromResponseH
  :: forall output input env m
   . ( FromJSON output
     , HasPriority input
     , HasPriority output
     , MonadCatch m
     , MonadEffects env m
     , MonadThrow m
     , Monoid output
     , ToRequest m input
     )
  => input
  -> m output
fromResponseH = Shared.fromResponseH @ResponseException @output handlers

fromValues_
  :: (FromJSON input, MonadCatch m, HasLogger env m, HasPriority input)
  => (input -> m ())
  -> [Value]
  -> m ()
fromValues_ = Shared.fromValues_ handlers

fromValues
  :: ( FromJSON input
     , MonadCatch m
     , HasLogger env m
     , HasPriority input
     , Monoid output
     )
  => (input -> m output)
  -> [Value]
  -> m [output]
fromValues = Shared.fromValues handlers

handlers :: (Monoid output, HasLogger env m) => [Handler m output]
handlers = Shared.handlers <>
  [ Handler $ \(e :: ResponseException) -> logData e >> pure mempty
  , Handler $ \(e :: UploadException)   -> logData e >> pure mempty
  ]
