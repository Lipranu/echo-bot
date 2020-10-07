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
import Control.Monad.State    ( MonadState, execStateT, evalStateT, runStateT, modify )
import Data.Aeson             ( FromJSON, Value )
import Data.Maybe             ( fromMaybe, listToMaybe )
import Data.Text.Extended     ( Text, showt )
import System.Random          ( randomIO )

-- TYPES -------------------------------------------------------------------

type AppReader r m = (SharedReader r m, VkReader r m)

-- FUNCTIONS ---------------------------------------------------------------

getLongPollServer
  :: (VkReader r m, MonadEffects r m, MonadThrow m)
  => m GetUpdates
getLongPollServer = mkGetUpdates <$> fromResponseR GetLongPollServer

getUpdates
  :: ( AppReader r m
     , MonadCatch m
     , MonadEffects r m
     , MonadRepetitions r m
     , MonadThrow m
     )
  => GetUpdates
  -> m ()
getUpdates gu = withLog requestAndDecode gu >>= \case
  Updates xs ts -> fromValues_ routeUpdate xs >> getUpdates gu { guTs = ts }
  OutOfDate ts  -> getUpdates gu { guTs = showt ts }
  rest          -> getLongPollServer >>= getUpdates

routeUpdate :: ( MonadRepetitions r m
               , AppReader r m
               , MonadEffects r m
               , MonadThrow m
               , MonadCatch m
               )
            => Update
            -> m ()
routeUpdate (NewMessage msg) = logData msg >> case getter msg of
  Nothing  -> processMessage msg
  Just cmd -> processCommand msg cmd $ mkContext msg
routeUpdate rest = logData rest

processMessage' upd = logData upd >> case upd of
  NewMessage msg -> do
    continue <- evalStateT (processCommand' $ getter msg) msg
    when continue $ do
      (attachments, message) <- runStateT (pure (0,msg)) msg
      pure ()
  rest -> pure mempty

processCommand'
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
processCommand' Nothing    = pure True
processCommand' (Just cmd) = logData cmd >> case cmd of
  UnknownCommand _ -> pure True
  Help             -> (unHelpText <$> obtain) >>= replyCommand >> pure False
  NewCount i       -> replyCommand ("Repeat count set to: " <> showt i)
                   >> pure False
  Repeat           -> do
    text    <- unRepeatText    <$> obtain
    def     <- unDefaultRepeat <$> obtain
    current <- getRepeats      =<< grab
    let repeats = fromMaybe def current
    replyCommand $ text <> "\nCurrent repeat count: " <> showt repeats
    pure False
  where replyCommand text = getName' >>= mkCommandReply' text >>= sendMessage

getName' :: ( Has FromId s
            , Has Context s
            , MonadEffects r m
            , VkReader r m
            , MonadState s m
            , MonadThrow m
            , MonadCatch m
            )
         => m (Maybe UserName)
getName' = grab >>= \case
  Private -> pure Nothing
  Chat    -> mkGetName' >>= fmap listToMaybe . fromResponseH

sendMessage :: (MonadEffects r m, MonadIO m, VkReader r m, MonadThrow m)
            => (Int -> SendMessage)
            -> m ()
sendMessage sm = do
  msg <- sm <$> liftIO randomIO
  fromResponse_ @MessageSended msg

processCommand :: ( MonadRepetitions r m
                  , AppReader r m
                  , MonadEffects r m
                  , MonadThrow m
                  , MonadCatch m
                  )
               => Message
               -> Command
               -> Context
               -> m ()
processCommand m command context = do
  performCommand
  name <- getName context $ mFromId m
  text <- getCommandText
  sendMessage
    $ mkCommandReply m
    $ mkCommandText context text name
    $ mFromId m
  where
    performCommand = logData command >> case command of
      NewCount i       -> putRepeats i $ getter m
      UnknownCommand c -> processMessage m
      _                -> pure ()

    getCommandText = case command of
      Help             -> unHelpText <$> obtain
      NewCount i       -> pure $ "Repeat count set to: " <> showt i
      UnknownCommand t -> pure "Unknown command"
      Repeat           -> do
        text    <- unRepeatText    <$> obtain
        def     <- unDefaultRepeat <$> obtain
        repeats <- fromMaybe def   <$> getRepeats (getter m)
        pure $ text <> "\nCurrent repeat count: " <> showt repeats

getName :: (MonadEffects r m, VkReader r m, MonadThrow m)
        => Context
        -> FromId
        -> m (Maybe UserName)
getName Private _   = pure Nothing
getName Chat fromId = listToMaybe <$> fromResponseR (mkGetName fromId)

processMessage :: ( Applicative m
                  , MonadEffects r m
                  , MonadRepetitions r m
                  , VkReader r m
                  , MonadThrow m
                  , MonadCatch m
                  )
               => Message
               -> m ()
processMessage m = do
  aState  <- execStateT
    (fromValues_ routeAttachment $ mAttachments m) $ mkState m
  (keyboard, repeats) <- findUser
  let sm = mkSendMessage m aState keyboard
  replicateM_ repeats $ sendMessage sm
  where findUser = getRepeats (getter m) >>= \case
          Nothing -> (mkKeyboard,) . unDefaultRepeat <$> obtain
          Just i  -> pure (Nothing, i)

routeAttachment :: ( Applicative m
                   , MonadEffects r m
                   , MonadIO m
                   , MonadState AttachmentsState m
                   , VkReader r m
                   , MonadThrow m
                   )
                => Attachment
                -> m ()
routeAttachment a = logData a >> case a of
  Graffiti          -> sendNotification "graffiti"
  Attachment   body -> addAttachment body
  Photo        body -> fromContext   body
  Document     body -> fromType      body
  AudioMessage body -> toUploadRequests >=> processDocument $ body
  Sticker      id   -> addSticker    id
  where fromContext x = grab >>= \case
          Private -> addAttachment x
          Chat    -> toUploadRequests >=> processDocument $ x
        fromType doc  = case dbType doc of
          "graffiti" -> sendNotification "graffiti"
          "photo"    -> toUploadRequests >=> processDocument $ docToPhoto doc
          _          -> toUploadRequests >=> processDocument $ doc

sendNotification :: ( MonadEffects r m
                    , MonadIO m
                    , MonadState AttachmentsState m
                    , MonadThrow m
                    , VkReader r m
                    )
                 => Text
                 -> m ()
sendNotification aType = do
  peerId    <- grab
  fromId    <- grab
  messageId <- grab
  context   <- grab
  name      <- getName context fromId
  sendMessage $ mkNotification context name messageId fromId peerId aType

addAttachment :: (ToAttachment a, MonadState AttachmentsState m)
              => a
              -> m ()
addAttachment x = modify $ \as ->
  as { asAttachments = toAttachment x : asAttachments as }

addSticker :: MonadState AttachmentsState m => Integer -> m ()
addSticker id = modify $ \as -> as { asSticker = Just id }

processDocument :: forall result env m
                 . ( MonadEffects env m
                   , MonadIO m
                   , MonadState AttachmentsState m
                   , VkReader env m
                   , MonadThrow m
                   , ToAttachment result
                   , HasPriority result
                   , FromJSON result
                   )
                => UploadRequests result
                -> m ()
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
     , HasLogger env m
     , HasPriority input
     , HasPriority output
     , MonadCatch m
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
