{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}

module Infrastructure.Requester
  ( MonadRequester (..)
  , Requester (..)
  , ToRequest (..)
  , DecodeException (..)
  , HasRequester

  , decode
  , mkRequester
  , parse
  , request
  , requestAndDecode
  ) where

-- IMPORTS -----------------------------------------------------------------

import Infrastructure.Logger
import Infrastructure.Has

import Control.Monad.Catch   ( Exception (..), MonadThrow (..), throwM )
import Control.Monad.Reader  ( MonadReader, lift )
import Control.Monad.State   ( StateT (..) )
import Data.Text.Encoding    ( decodeUtf8 )
import Data.Text.Extended    ( Text )
import GHC.Generics          ( Generic )

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Extended   as Text
import qualified Network.HTTP.Client  as HTTP

-- CLASSES -----------------------------------------------------------------

class Monad m => MonadRequester m where
  requester :: HTTP.Manager
            -> HTTP.Request
            -> m (HTTP.Response LBS.ByteString)

class Monad m => ToRequest m a where
  toRequest :: a -> m HTTP.Request

-- TYPES AND INSTANCES -----------------------------------------------------

type HasRequester r m =
  ( MonadRequester m
  , Has (Requester m) r
  , MonadReader r m
  )

newtype Requester m = Requester
  { runRequester :: HTTP.Request -> m (HTTP.Response LBS.ByteString) }

instance MonadRequester m => MonadRequester (StateT s m) where
   requester m = lift . requester m

instance (Has (Requester m) r, MonadReader r m)
  => Has (Requester (StateT s m)) r where
  getter env = Requester $ \req -> StateT $ \s ->
    (,s) <$> runRequester (getter env) req

data DecodeException = DecodeException
  { errorMessage :: Text
  , source       :: Text
  } deriving stock (Generic, Show)
    deriving anyclass Exception
    deriving Loggable via LogError DecodeException

-- FUNCTIONS ---------------------------------------------------------------

mkRequester :: MonadRequester m => HTTP.Manager -> Requester m
mkRequester = Requester . requester

request :: (ToRequest m a, HasRequester r m, MonadThrow m)
        => a
        -> m LBS.ByteString
request x = do
  requester <- obtain
  request   <- toRequest x
  result    <- runRequester requester request
  return $ HTTP.responseBody result

decode :: (MonadThrow m, Aeson.FromJSON a) => LBS.ByteString -> m a
decode bs = case Aeson.eitherDecode bs of
  Right r -> return r
  Left  e -> throwM $ DecodeException (Text.pack e)
                    $ decodeUtf8
                    $ LBS.toStrict bs

requestAndDecode
  :: (MonadThrow m, ToRequest m a, Aeson.FromJSON b, HasRequester r m)
  => a
  -> m b
requestAndDecode req = request req >>= decode

parse :: (MonadThrow m, Aeson.FromJSON a) => Aeson.Value -> m a
parse value = case Aeson.fromJSON value of
  Aeson.Success result -> return result
  Aeson.Error error    -> throwM $ DecodeException (Text.pack error)
                                 $ Text.showt value
