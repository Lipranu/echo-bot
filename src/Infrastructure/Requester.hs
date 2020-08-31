{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}

module Infrastructure.Requester
  ( MonadRequester (..)
  , Requester
  , ToRequest (..)
  , Result (..)

  , decode
  , mkRequester
  , request
  , requestAndDecode
  ) where

-- IMPORTS ---------------------------------------------------------------------

import Internal

import Control.Monad.Reader         ( MonadReader, asks )
import Data.Aeson                   ( FromJSON, eitherDecode )
import Data.Text                    ( Text )
import Data.Text.Encoding           ( decodeUtf8 )
import Network.HTTP.Client.Extended ( Request, Response
                                    , Manager, HttpException )

import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Text                    as Text
import qualified Network.HTTP.Client.Extended as HTTP

-- CLASSES ---------------------------------------------------------------------

class Monad m => MonadRequester m where
  requester :: Manager -> Request -> m RequestResult

class ToRequest a where
  toRequest :: a -> Request

-- TYPES AND INSTANCES ---------------------------------------------------------

type RequestResult = Either HttpException (Response LBS.ByteString)

newtype Requester m = Requester { unRequester :: Request -> m RequestResult }

data Result a
  = Result a
  | DecodeError Text Text
  | RequestError Text
  deriving (Show, Eq)

-- FUNCTIONS -------------------------------------------------------------------

mkRequester :: MonadRequester m => Manager -> Requester m
mkRequester = Requester . requester

request :: (ToRequest a, Has (Requester m) r, MonadReader r m)
        => a
        -> m (Result LBS.ByteString)
request r = do
  req    <- asks getter
  result <- unRequester req $ toRequest r
  case result of
    Left  e -> return $ RequestError $ Text.pack $ show e
    Right r -> return $ Result $ HTTP.responseBody r

decode :: forall a . FromJSON a => Result LBS.ByteString -> Result a
decode (Result bs)        = case eitherDecode bs of
  Left  e -> DecodeError (Text.pack e) $ decodeUtf8 $ LBS.toStrict bs
  Right r -> Result r
decode (RequestError e)   = RequestError e
decode (DecodeError  e t) = DecodeError  e t

requestAndDecode
  :: forall b a r m .
  ( ToRequest a, FromJSON b, Has (Requester m) r, MonadReader r m )
  => a
  -> m (Result b)
requestAndDecode req = decode <$> request req
