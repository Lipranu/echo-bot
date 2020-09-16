{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module App.Vk.Routes
  ( getLongPollServer
  , start
  ) where

-- IMPORTS -----------------------------------------------------------------

import App.Shared
import App.Shared.Repetition
import App.Shared.Routes
import App.Vk.Converters
import Infrastructure.Logger
import Infrastructure.Requester
import Internal

import Control.Monad          ( replicateM_ )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.State    ( MonadState, execStateT )
import Data.IORef             ( IORef )
import Data.Text.Extended     ( Text, showt )
import System.Random          ( randomIO )

-- FUNCTIONS ---------------------------------------------------------------

getLongPollServer :: ( Has (IORef Repetitions) r
                     , Has DefaultRepeat r
                     , Has HelpText r
                     , Has RepeatText r
                     , MonadEffects r m
                     , MonadIO m
                     , VkReader r m
                     )
                  => m ()
getLongPollServer = handleErrorRequest GetLongPollServer
  $ getUpdates . mkGetUpdates

getUpdates :: ( Has (IORef Repetitions) r
              , Has DefaultRepeat r
              , Has HelpText r
              , Has RepeatText r
              , MonadEffects r m
              , MonadIO m
              , VkReader r m
              )
           => GetUpdates
           -> m ()
getUpdates gu = handleErrorRequest gu $ routeUpdates gu

routeUpdates :: ( Has (IORef Repetitions) r
                , Has DefaultRepeat r
                , Has HelpText r
                , Has RepeatText r
                , MonadEffects r m
                , MonadIO m
                , VkReader r m
                )
             => GetUpdates
             -> Updates
             -> m ()
routeUpdates gu (Updates upd ts) = do
    traverseHandle processUpdate $ parse <$> upd
    getUpdates gu { guTs = ts }
routeUpdates gu (OutOfDate ts) = getUpdates gu { guTs = showt ts }
routeUpdates _ _               = getLongPollServer

processUpdate :: ( Has (IORef Repetitions) r
                 , Has DefaultRepeat r
                 , Has HelpText r
                 , Has RepeatText r
                 , MonadEffects r m
                 , MonadIO m
                 , VkReader r m
                 )
              => Update
              -> m ()
processUpdate (NewMessage m) = case mPayload m of
  Nothing    -> withLog processMessage m
  Just "101" -> withLog processHelp m
  Just "102" -> withLog processRepeat m
  Just "201" -> processIndex m 1
  Just "202" -> processIndex m 2
  Just "203" -> processIndex m 3
  Just "204" -> processIndex m 4
  Just "205" -> processIndex m 5
  Just text  -> logWarning ("unknown payload: " <> text)
             >> withLog processMessage m
processUpdate _ = logWarning ("NOT IMPLEMENTED" :: Text)

checkChat :: (MonadEffects r m, VkReader r m)
          => Message
          -> m (Maybe UserName)
checkChat m | mPeerId m == mFromId m = return Nothing
            | otherwise              = getName $ mkGetName m

getName :: (MonadEffects r m, VkReader r m)
        => GetName -> m (Maybe UserName)
getName gn = withLog requestAndDecode gn >>= \case
  Result (Success (x:_)) -> return $ Just x
  error -> logWarning error >> return Nothing

processIndex :: ( Has (IORef Repetitions) r
                , MonadEffects r m
                , MonadIO m
                , VkReader r m
                )
             => Message
             -> Int
             -> m ()
processIndex m i = putRepeats m i
  >>  checkChat m
  >>= sendMessage . mkIndexReply i m

processHelp :: (Has HelpText r, MonadEffects r m, MonadIO m, VkReader r m)
            => Message
            -> m ()
processHelp m = checkChat m >>= mkHelpReply m >>= sendMessage

processRepeat :: ( Has RepeatText r
                 , MonadEffects r m
                 , MonadIO m
                 , VkReader r m
                 )
              => Message
              -> m ()
processRepeat m = checkChat m >>= mkRepeatReply m >>= sendMessage

processMessage :: ( Has (IORef Repetitions) r
                  , Has DefaultRepeat r
                  , MonadEffects r m
                  , MonadIO m
                  , VkReader r m
                  )
               => Message
               -> m ()
processMessage m = do
  aState  <- execStateT
    (traverseHandle routeAttachment $ parse <$> mAttachments m) $ mkState m
  repeats <- getRepeats m
  replicateM_ repeats $ sendMessage $ mkSendMessage m aState repeats

sendMessage :: (MonadEffects r m, MonadIO m, VkReader r m)
            => (Int -> SendMessage)
            -> m ()
sendMessage sm = do
  sendm <- sm <$> liftIO randomIO
  handleWarningRequest @MessageSended sendm endRoute

routeAttachment :: ( MonadEffects r m
                   , MonadIO m
                   , MonadState AttachmentsState m
                   , VkReader r m
                   )
                => Attachment
                -> m ()
routeAttachment (Attachment body) = withLog addAttachment body
routeAttachment (Wall       body) = withLog addAttachment body
routeAttachment (Document   body) = withLog processDocument body
routeAttachment (Sticker      id) = addSticker id

processDocument :: ( MonadEffects r m
                   , MonadIO m
                   , MonadState AttachmentsState m
                   , VkReader r m
                   )
                => DocumentBody -> m ()
processDocument doc = do
  gUploadServer <- mkGetUploadServer
  handleWarningRequest gUploadServer $ getFile doc

getFile :: ( MonadEffects r m
           , MonadIO m
           , MonadState AttachmentsState m
           , VkReader r m
           )
        => DocumentBody
        -> UploadServer
        -> m ()
getFile doc us = do
  file <- request $ mkGetFile doc
  handleWarningR (uploadDocument . mkUploadFile doc us) $ RawFile <$> file

uploadDocument :: ( MonadEffects r m
                  , MonadIO m
                  , MonadState AttachmentsState m
                  , VkReader r m
                  )
               => UploadFile
               -> m ()
uploadDocument uFile = handleWarningRequest uFile
  $ saveFile . mkSaveFile uFile

saveFile :: (MonadEffects r m, MonadState AttachmentsState m, VkReader r m)
         => SaveFile
         -> m ()
saveFile sf = handleWarningRequest @FileSaved sf addAttachment

--endRoute :: Monad m => a -> m ()
--endRoute = const (return ())
--
---- TODO: move all handlers to shared module
--handle :: (HasLogger r m, HasPriority a)
--       => (Result (Response a) -> m ())  -- logger for input
--       -> m ()                           -- additional logger
--       -> (a -> m ())                    -- function for handled input
--       -> Result (Response a)            -- input
--       -> m ()                           -- phantom result
--handle _ _ route (Result (Success x)) = logData x >> route x
--handle logger1 logger2 _ error = logger1 error >> logger2
--
--handleR :: (HasLogger r m, HasPriority a)
--       => (Result a -> m ())
--       -> m ()
--       -> (a -> m ())
--       -> Result a
--       -> m ()
--handleR _ _ route (Result x) = logData x >> route x
--handleR logger1 logger2 _ error = logger1 error >> logger2
--
--handleWarningR :: (HasLogger r m, HasPriority a)
--       => (a -> m ())
--       -> Result a
--       -> m ()
--handleWarningR = handleR logWarning (return ())
--
--handleError, handleWarning
--  :: (HasLogger r m, HasPriority input)
--  => (input -> m ())
--  -> Result (Response input)
--  -> m ()
--handleError   = handle logError shutdown
--handleWarning = handle logWarning (return ())
--
--withLog :: (HasLogger r m, HasPriority a) => (a -> m b) -> a -> m b
--withLog f x = logData x >> f x
--
--requestWithLog :: ( Aeson.FromJSON output
--                  , HasPriority input
--                  , MonadEffects r m
--                  , ToRequest m r input
--                  )
--               => input
--               -> m (Result (Response output))
--requestWithLog = withLog requestAndDecode
--
--handleErrorRequest, handleWarningRequest
--  :: forall output input r m
--   . ( Aeson.FromJSON output
--     , HasPriority input
--     , HasPriority output
--     , MonadEffects r m
--     , ToRequest m r input
--     )
--  => input
--  -> (output -> m ())
--  -> m ()
--handleErrorRequest   x f = requestWithLog x >>= handleError f
--handleWarningRequest x f = requestWithLog x >>= handleWarning f
--
--traverseHandle :: (HasLogger r m, HasPriority input)
--               => (input -> m ())
--               -> [Result input]
--               -> m ()
--traverseHandle f = traverse_ (handleWarningR f)
--
--start, shutdown :: HasLogger r m => m ()
--start    = logInfo ("Application getting started" :: Text)
--shutdown = logInfo ("Application shut down" :: Text)
