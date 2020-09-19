{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Vk.Routes
  ( getLongPollServer
  , getUpdates
  , processUpdates
  , handlers
  , start
  , shutdown
  ) where

-- IMPORTS -----------------------------------------------------------------

import App.Shared
import App.Shared.Repetition
import App.Shared.Routes
import App.Vk.Converters
import Infrastructure.Logger hiding ( Priority (..) )
import Infrastructure.Requester
import Internal

import Data.Foldable          ( traverse_ )
import Control.Monad          ( replicateM_ )
import Control.Monad.Catch    ( Handler (..), Exception, MonadThrow, MonadCatch, throwM, catches )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.State    ( MonadState, execStateT, get )
import Control.Monad.Cont     ( ContT (..), MonadCont (..), lift )
import Data.IORef             ( IORef )
import Data.Text.Extended     ( Text, showt )
import System.Random          ( randomIO )
import Network.HTTP.Client    ( HttpException )

import Data.Aeson             ( FromJSON, Value )

-- FUNCTIONS ---------------------------------------------------------------

getLongPollServer :: (VkReader r m, MonadEffects r m, MonadThrow m)
                  => m GetUpdates
getLongPollServer = mkGetUpdates <$> withLog fromResponseR GetLongPollServer

getUpdates :: (VkReader r m, MonadEffects r m, MonadThrow m)
           => ([Value] -> m b)
           -> GetUpdates
           -> m ()
getUpdates f gu = withLog requestAndDecode gu >>= \case
  Updates upds ts -> f upds >> getUpdates f gu { guTs = ts }
  OutOfDate ts    -> getUpdates f gu { guTs = showt ts }
  rest            -> getLongPollServer >>= getUpdates f

processUpdates :: (HasLogger r m, MonadCatch m)  => [Value] -> m ()
processUpdates = traverse_ f
  where f x = (parse x >>= processUpdate) `catches` handlers

processUpdate :: HasLogger r m => Update -> m ()
processUpdate (NewMessage m) = logData m >> routeUpdate m
processUpdate rest = logData rest

routeUpdate :: HasLogger r m => Message -> m ()
routeUpdate m = case mPayload m of
 Nothing    -> logDebug ("Nothing" :: Text)--withLog processMessage m
 Just "101" -> logDebug ("101" :: Text)--withLog processHelp m
 Just "102" -> logDebug ("102" :: Text)--withLog processRepeat m
 Just "201" -> logDebug ("201" :: Text)--processIndex m 1
 Just "202" -> logDebug ("202" :: Text)--processIndex m 2
 Just "203" -> logDebug ("203" :: Text)--processIndex m 3
 Just "204" -> logDebug ("204" :: Text)--processIndex m 4
 Just "205" -> logDebug ("205" :: Text)--processIndex m 5
 Just text  -> logDebug text --logWarning ("unknown payload: " <> text)
           --  >> withLog processMessage m
--getLongPollServer :: ( Has (IORef Repetitions) r
--                     , Has DefaultRepeat r
--                     , Has HelpText r
--                     , Has RepeatText r
--                     , MonadEffects r m
--                     , MonadIO m
--                     , VkReader r m
--                     --, MonadCont m
--                     )
--                  => m ()
--getLongPollServer = handleErrorRequest GetLongPollServer
--  $ getUpdates . mkGetUpdates
--
--getUpdates :: ( Has (IORef Repetitions) r
--              , Has DefaultRepeat r
--              , Has HelpText r
--              , Has RepeatText r
--              , MonadEffects r m
--              , MonadIO m
--              , VkReader r m
--              )
--           => GetUpdates
--           -> m ()
--getUpdates gu = handleErrorRequest gu $ routeUpdates gu
--
--routeUpdates :: ( Has (IORef Repetitions) r
--                , Has DefaultRepeat r
--                , Has HelpText r
--                , Has RepeatText r
--                , MonadEffects r m
--                , MonadIO m
--                , VkReader r m
--                )
--             => GetUpdates
--             -> Updates
--             -> m ()
--routeUpdates gu (Updates upd ts) = do
--    traverseHandle processUpdate $ parse <$> upd
--    getUpdates gu { guTs = ts }
--routeUpdates gu (OutOfDate ts) = getUpdates gu { guTs = showt ts }
--routeUpdates _ _               = getLongPollServer
--
--processUpdate :: ( Has (IORef Repetitions) r
--                 , Has DefaultRepeat r
--                 , Has HelpText r
--                 , Has RepeatText r
--                 , MonadEffects r m
--                 , MonadIO m
--                 , VkReader r m
--                 )
--              => Update
--              -> m ()
--processUpdate (NewMessage m) = case mPayload m of
--  Nothing    -> withLog processMessage m
--  Just "101" -> withLog processHelp m
--  Just "102" -> withLog processRepeat m
--  Just "201" -> processIndex m 1
--  Just "202" -> processIndex m 2
--  Just "203" -> processIndex m 3
--  Just "204" -> processIndex m 4
--  Just "205" -> processIndex m 5
--  Just text  -> logWarning ("unknown payload: " <> text)
--             >> withLog processMessage m
--processUpdate _ = logWarning ("NOT IMPLEMENTED" :: Text)
--
--checkChat :: (MonadEffects r m, VkReader r m)
--          => Message
--          -> m (Maybe UserName)
--checkChat m | mPeerId m == mFromId m = return Nothing
--            | otherwise              = getName $ mkGetName m
--
--getName :: (MonadEffects r m, VkReader r m)
--        => GetName -> m (Maybe UserName)
--getName gn = withLog requestAndDecode gn >>= \case
--  Result (Success (x:_)) -> return $ Just x
--  error -> logWarning error >> return Nothing
--
--processIndex :: ( Has (IORef Repetitions) r
--                , MonadEffects r m
--                , MonadIO m
--                , VkReader r m
--                )
--             => Message
--             -> Int
--             -> m ()
--processIndex m i = putRepeats m i
--  >>  checkChat m
--  >>= sendMessage . mkIndexReply i m
--
--processHelp :: (Has HelpText r, MonadEffects r m, MonadIO m, VkReader r m)
--            => Message
--            -> m ()
--processHelp m = checkChat m >>= mkHelpReply m >>= sendMessage
--
--processRepeat :: ( Has RepeatText r
--                 , MonadEffects r m
--                 , MonadIO m
--                 , VkReader r m
--                 )
--              => Message
--              -> m ()
--processRepeat m = checkChat m >>= mkRepeatReply m >>= sendMessage
--
--processMessage :: ( Has (IORef Repetitions) r
--                  , Has DefaultRepeat r
--                  , MonadEffects r m
--                  , MonadIO m
--                  , VkReader r m
--                  )
--               => Message
--               -> m ()
--processMessage m = do
--  aState  <- execStateT
--    (traverseHandle routeAttachment $ parse <$> mAttachments m) $ mkState m
--  repeats <- getRepeats m
--  replicateM_ repeats $ sendMessage $ mkSendMessage m aState repeats
--
--sendMessage :: (MonadEffects r m, MonadIO m, VkReader r m)
--            => (Int -> SendMessage)
--            -> m ()
--sendMessage sm = do
--  sendm <- sm <$> liftIO randomIO
--  handleWarningRequest @MessageSended sendm endRoute
--
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
fromResponseR input = fromResponse @ResponseException @output input
fromResponseU input = fromResponse @UploadException   @output input

handlers :: HasLogger r m => [Handler m () ]
handlers = sharedHandlers <>
  [ Handler $ \(e :: ResponseException) -> logError $ toLog e
  , Handler $ \(e :: UploadException)   -> logError $ toLog e
  ]