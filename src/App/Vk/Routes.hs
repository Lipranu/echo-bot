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
import App.Shared.Routes

import App.Vk.Config     ( VkReader )
import App.Vk.Converters
import App.Vk.Requests
import App.Vk.Responses

import Infrastructure.Logger    hiding ( Priority (..) )
import Infrastructure.Requester
import Internal

import Control.Monad          ( replicateM_ )
import Control.Monad.Catch    ( Handler (..), MonadThrow, MonadCatch )
import Control.Monad.IO.Class ( MonadIO (..) )
import Control.Monad.State    ( MonadState, execStateT, modify )
import Data.Aeson             ( FromJSON, Value )
import Data.Maybe             ( fromMaybe, listToMaybe )
import Data.Text.Extended     ( showt )
import System.Random          ( randomIO )

-- TYPES -------------------------------------------------------------------

type AppReader r m = (SharedReader r m, VkReader r m)

-- FUNCTIONS ---------------------------------------------------------------

getLongPollServer :: (VkReader r m, MonadEffects r m, MonadThrow m)
                  => m GetUpdates
getLongPollServer = mkGetUpdates <$> withLog fromResponseR GetLongPollServer

getUpdates :: ( AppReader r m
              , MonadCatch m
              , MonadEffects r m
              , MonadRepetitions r m
              , MonadThrow m
              )
           => GetUpdates
           -> m ()
getUpdates gu = withLog requestAndDecode gu >>= \case
  Updates xs ts -> fromValues routeUpdate xs >> getUpdates gu { guTs = ts }
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

sendMessage :: (MonadEffects r m, MonadIO m, VkReader r m, MonadThrow m)
            => (Int -> SendMessage)
            -> m ()
sendMessage sm = sm
  <$> liftIO randomIO
  >>= withLog_ (fromResponseR @MessageSended)

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
processCommand m command context= do
  performCommand
  name <- getName
  text <- getCommandText
  sendMessage $ mkCommandReply m $ mkCommandText m context name text
  where
    performCommand = logData command >> case command of
      NewCount i       -> putRepeats m i
      UnknownCommand c -> processMessage m
      _                -> pure ()

    getName = case context of
      Private -> pure Nothing
      Chat    -> listToMaybe <$> withLog fromResponseR (mkGetName m)

    getCommandText = case command of
      Help             -> unHelpText <$> obtain
      NewCount i       -> pure $ "Repeat count set to: " <> showt i
      UnknownCommand t -> pure "Unknown command"
      Repeat           -> do
        text    <- unRepeatText    <$> obtain
        def     <- unDefaultRepeat <$> obtain
        repeats <- fromMaybe def   <$> getRepeats m
        pure $ text <> "\nCurrent repeat count: " <> showt repeats

processMessage :: ( MonadEffects r m
                  , MonadRepetitions r m
                  , VkReader r m
                  , MonadThrow m
                  , MonadCatch m
                  )
               => Message
               -> m ()
processMessage m = do
  aState  <- execStateT
    (fromValues routeAttachment $ mAttachments m) $ mkState m
  (keyboard, repeats) <- findUser
  let sm = mkSendMessage m aState keyboard
  replicateM_ repeats $ sendMessage sm
  where findUser = getRepeats m >>= \case
          Nothing -> (mkKeyboard,) . unDefaultRepeat <$> obtain
          Just i  -> pure (Nothing, i)

routeAttachment :: ( MonadEffects r m
                   , MonadIO m
                   , MonadState AttachmentsState m
                   , VkReader r m
                   , MonadThrow m
                   )
                => Attachment
                -> m ()
routeAttachment a = logData a >> case a of
  Attachment body -> addAttachment body
  Wall body       -> addAttachment body
  Photo body      -> fromContext body
  Document body   -> processDocument $ toUploadRequests body
  Sticker id      -> addSticker id
  where fromContext a = grab >>= \case
          Private -> addAttachment a
          Chat    -> processDocument $ toUploadRequests a

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
  server   <- withLog fromResponseR getUploadServer
  file     <- inputLog request getFile
  uploaded <- withLog fromResponseU $ uploadFile server file
  saved    <- withLog (fromResponseR @result) $ saveFile uploaded
  addAttachment saved

fromResponseR, fromResponseU
  :: forall output input env m
   . (ToRequest m input, FromJSON output, MonadThrow m, HasRequester env m)
  => input
  -> m output
fromResponseR = fromResponse @ResponseException @output
fromResponseU = fromResponse @UploadException   @output

fromValues :: (FromJSON a, MonadCatch m, HasLogger r m)
           => (a -> m ())
           -> [Value]
           -> m ()
fromValues = handleValues handlers

handlers :: HasLogger r m => [Handler m () ]
handlers = sharedHandlers <>
  [ Handler $ \(e :: ResponseException) -> logError $ toLog e
  , Handler $ \(e :: UploadException)   -> logError $ toLog e
  ]
