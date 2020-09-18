{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Vk.Routes
  ( getLongPollServer
  , getUpdates
  --, start
  --, endRoute
  --, shutdown
  ) where

-- IMPORTS -----------------------------------------------------------------

import App.Shared
import App.Shared.Repetition
import App.Shared.Routes
import App.Vk.Converters
import Infrastructure.Logger hiding ( Priority (..) )
import Infrastructure.Requester
import Internal

import Control.Monad          ( replicateM_ )
import Control.Monad.Catch    ( Exception, MonadThrow, throwM )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.State    ( MonadState, execStateT, get )
import Control.Monad.Cont     ( ContT (..), MonadCont (..), lift )
import Data.IORef             ( IORef )
import Data.Text.Extended     ( Text, showt )
import System.Random          ( randomIO )

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


handleResponse :: (Exception e, FromJSON e, FromJSON a, MonadThrow m)
               => (Response e a) -> m a
handleResponse (Success x) = return x
handleResponse (Error   e) = throwM e

withLog :: (HasPriority a, HasPriority b, HasLogger r m)
        => (a -> m b)
        -> a
        -> m b
withLog f x = logData x >> f x >>= \y -> logData y >> return y

fromResponseR, fromResponseU
  :: forall b a e m r
   . (ToRequest m a, FromJSON b, MonadThrow m, HasRequester r m)
  => a
  -> m b
fromResponseR x = requestAndDecode x >>= handleResponse @ResponseException
fromResponseU x = requestAndDecode x >>= handleResponse @UploadException
--  \case
--  Success x -> return x
--  Error   e -> throwM e
--fromResponseE = fromResponse @ResponseException
--fromResponseU = fromResponse @UploadException
--withSuccess :: Response a -> m a
--withSuccess (Success x) = return x
--withSuccess (
--getLongPollServer' :: ( --Has (IORef Repetitions) r
--                     , Has DefaultRepeat r
--                     , Has HelpText r
--                     , Has RepeatText r
--                      MonadEffects r m
--                     , MonadIO m
--                    ,  VkReader r m
--  , MonadCont m
--                     )
--                  => (GetUpdates -> m ()) -> m () --GetUpdates
--getLongPollServer' f = handleErrorReq (f . mkGetUpdates) GetLongPollServer--(f . mkGetUpdates) GetLongPollServer
--
--handleError' :: MonadCont m => ((err -> m a) -> m a) -> (err -> m a) -> m a
--handleError' c h = callCC $ \ok -> do
--  err <- callCC $ \notOk -> do
--    x <- c notOk
--    ok x
--  h err
--
--handleReq :: forall b a r m . (ToRequest m r a, MonadCont m, FromJSON b, MonadRequester m, Has (Requester m) r) => (b -> m ()) -> a -> (Result (Response b) -> m ()) -> m ()
--handleReq f a throw = requestAndDecode a >>= \case
--  Result (Success x) -> f x
--  error -> throw error
--
----handleErrorReq :: (ToRequest m r a, MonadCont m, FromJSON b, MonadRequester m, Has (Requester m) r) => (Result (Response b) -> m b) -> a -> m b
--handleErrorReq :: forall b a r m . (FromJSON b, HasPriority b, ToRequest m r a, MonadCont m, MonadEffects r m) => (b -> m ()) -> a -> m ()
--handleErrorReq f x = handleError' (handleReq f x) (logError) --const
--
--instance Loggable () where
--  toLog _ = "()"
--handleError :: (MonadCont m, HasLogger r m, Loggable a)
--            => (a -> m ())
--            -> Result (Response a)
--            -> m ()
--handleError f v = callCC $ \k -> case v of
--  Result (Success x) -> f x
--  error -> logError error >> k ()
--  --e@(DecodeError err t) -> k e
----handleError _ (Result (Success x)) = return x
----handleError abort err = logError err >> abort
--
--handleErrReq :: ( Loggable b
--                , MonadCont m
--                , MonadEffects r m
--                , ToRequest m r a
--                , FromJSON b)
--            -- => -- (b -> m r)
--             => (b -> m ())
--             -> a
--             -> m ()
--handleErrReq f v = requestAndDecode v >>= handleError f
--
--getUpdates' :: ( --Has (IORef Repetitions) r
--              -- Has DefaultRepeat r
--              -- Has HelpText r
--              -- Has RepeatText r
--               MonadEffects r m
--              -- MonadIO m
--              -- VkReader r m
--               , MonadCont m
--              )
--           => (GetUpdates -> Updates -> m ())
--           -> GetUpdates
--           -> m ()
--getUpdates' k gu = handleErrReq (k gu) gu --undefined--handleErrorRequest gu $ k gu u
--
----routeUpdates' :: ( Has (IORef Repetitions) r
----                , Has DefaultRepeat r
----                , Has HelpText r
----                , Has RepeatText r
----                , MonadEffects r m
----                , MonadIO m
----                , VkReader r m
----                )
----             => (Updates -> m ())
----             -> (GetUpdates -> m ())
----             -> (GetUpdates -> m ())
----             -> GetUpdates
----             -> Updates
----             -> m ()
----routeUpdates' k1 k2 k3 gu (Updates upd ts) = do
----    traverseHandle processUpdate $ parse <$> upd
----    getUpdates gu { guTs = ts }
----routeUpdates' gu (OutOfDate ts) = getUpdates gu { guTs = showt ts }
----routeUpdates' _ _               = getLongPollServer
--
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
