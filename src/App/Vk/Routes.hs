{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

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
import Control.Monad.State    ( MonadState, execStateT, get, modify )
import Data.Aeson             ( FromJSON, Value )
import Data.Maybe             ( fromMaybe, listToMaybe )
import Data.Text.Extended     ( Text, showt )
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

addAttachment :: (Convertible a Text, MonadState AttachmentsState m)
              => a
              -> m ()
addAttachment x = modify $ \as ->
  as { asAttachments = convert x : asAttachments as }

addSticker :: MonadState AttachmentsState m => Integer -> m ()
addSticker id = modify $ \as -> as { asSticker = Just id }

----------------------------------------------------------------------------

processMessage :: ( MonadEffects r m
                  , MonadRepetitions r m
                  , VkReader r m
                  , MonadThrow m
                  )
               => Message
               -> m ()
processMessage m = do
 -- aState  <- execStateT
 --   (traverseHandle routeAttachment $ parse <$> mAttachments m) $ mkState m
  (keyboard, repeats) <- findUser
  replicateM_ repeats $ sendMessage $ mkSendMessage m (mkState m) keyboard
  where findUser = getRepeats m >>= \case
          Nothing -> (mkKeyboard,) <$> unDefaultRepeat <$> obtain
          Just i  -> pure (Nothing, i)

--routeAttachment :: ( MonadEffects r m
--                   , MonadIO m
--                   , MonadState AttachmentsState m
--                   , VkReader r m
--                   )
--                => Attachment
--                -> m ()
--routeAttachment (Attachment body) = withLog addAttachment body
--routeAttachment (Wall       body) = withLog addAttachment body
--routeAttachment (Document   body) = withLog processDocument body
--routeAttachment (Sticker      id) = addSticker id
--
--processDocument :: ( MonadEffects r m
--                   , MonadIO m
--                   , MonadState AttachmentsState m
--                   , VkReader r m
--                   )
--                => DocumentBody -> m ()
--processDocument doc = do
--  gUploadServer <- mkGetUploadServer
--  handleWarningRequest gUploadServer $ getFile doc
--
--getFile :: ( MonadEffects r m
--           , MonadIO m
--           , MonadState AttachmentsState m
--           , VkReader r m
--           )
--        => DocumentBody
--        -> UploadServer
--        -> m ()
--getFile doc us = do
--  file <- request $ mkGetFile doc
--  handleWarningR (uploadDocument . mkUploadFile doc us) $ RawFile <$> file
--
--uploadDocument :: ( MonadEffects r m
--                  , MonadIO m
--                  , MonadState AttachmentsState m
--                  , VkReader r m
--                  )
--               => UploadFile
--               -> m ()
--uploadDocument uFile = handleWarningRequest uFile
--  $ saveFile . mkSaveFile uFile
--
--saveFile :: (MonadEffects r m, MonadState AttachmentsState m, VkReader r m)
--         => SaveFile
--         -> m ()
--saveFile sf = handleWarningRequest @FileSaved sf addAttachment

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
