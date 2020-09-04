{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Infrastructure.Requester
  ( MonadRequester (..)
  , Requester
  , ToRequest (..)
  , Result (..)

  , decode
  , mkRequester
  , parse
  , request
  , requestAndDecode
  ) where

-- IMPORTS -----------------------------------------------------------------

import Infrastructure.Logger        ( Loggable (..) )
import Internal

import Control.Monad.Reader         ( MonadReader )
import Data.Text.Extended           ( Text )
import Data.Text.Encoding           ( decodeUtf8 )

import qualified Data.Aeson                   as Aeson
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Text.Extended           as Text
import qualified Network.HTTP.Client.Extended as HTTP

-- CLASSES -----------------------------------------------------------------

class Monad m => MonadRequester m where
  requester :: HTTP.Manager -> HTTP.Request -> m RequestResult

class MonadReader r m => ToRequest m r a where
  toRequest :: a -> m HTTP.Request

-- TYPES AND INSTANCES -----------------------------------------------------

type RequestResult
  = Either HTTP.HttpException (HTTP.Response LBS.ByteString)

newtype Requester m
  = Requester { unRequester :: HTTP.Request -> m RequestResult }

data Result a
  = Result a
  | DecodeError Text Text
  | RequestError HTTP.HttpException
  deriving Show

instance Loggable a => Loggable (Result a) where
  toLog (Result result) = toLog result

  toLog (DecodeError err bs) = "An error occurred during decoding:\n\
    \ | Error Message: " <> err <> "\n\
    \ | Source: "        <> bs

  toLog (RequestError err) = "An error occurred during request:\n\
    \ | Error: " <> toLog err

-- FUNCTIONS ---------------------------------------------------------------

mkRequester :: MonadRequester m => HTTP.Manager -> Requester m
mkRequester = Requester . requester

request :: (ToRequest m r a, Has (Requester m) r, MonadReader r m)
        => a
        -> m (Result LBS.ByteString)
request r = do
  requester <- obtain
  request   <- toRequest r
  result    <- unRequester requester request
  case result of
    Left  e -> return $ RequestError e
    Right r -> return $ Result $ HTTP.responseBody r

decode :: Aeson.FromJSON a => Result LBS.ByteString -> Result a
decode (Result bs)        = case Aeson.eitherDecode bs of
  Left  e -> DecodeError (Text.pack e) $ decodeUtf8 $ LBS.toStrict bs
  Right r -> Result r
decode (RequestError e)   = RequestError e
decode (DecodeError  e t) = DecodeError  e t

requestAndDecode :: ( ToRequest m r a
                    , Aeson.FromJSON b
                    , Has (Requester m) r
                    , MonadReader r m
                    ) => a
                      -> m (Result b)
requestAndDecode req = decode <$> request req

parse :: Aeson.FromJSON a => Aeson.Value -> Result a
parse value = case Aeson.fromJSON value of
  Aeson.Success result -> Result result
  Aeson.Error error    -> DecodeError (Text.pack error) $ Text.showt value
