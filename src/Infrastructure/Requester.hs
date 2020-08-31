{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

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

import Internal                     ( Has (..) )

import Control.Monad.Reader         ( MonadReader, asks )
import Data.Aeson                   ( FromJSON, eitherDecode )
import Data.Text                    (Text)
import Data.Text.Encoding           (decodeUtf8)
import Network.HTTP.Client.Extended ( Request, Response
                                    , Manager, HttpException )

import qualified Data.ByteString.Lazy         as BSL
import qualified Data.Text                    as Text
import qualified Network.HTTP.Client.Extended as HTTP

-- CLASSES ---------------------------------------------------------------------

class Monad m => MonadRequester m where
  requester :: Manager
            -> Request
            -> m (Either HttpException (Response BSL.ByteString))

class ToRequest a where
  toRequest :: a -> Request

-- TYPES AND INSTANCES ---------------------------------------------------------

type HasRequester r m = (Has (Requester m) r, MonadReader r m, MonadRequester m)

newtype Requester m = Requester
  { unRequester :: Request
                -> m (Either HttpException (Response BSL.ByteString))
  }

data Result a
  = Result a
  | DecodeError Text Text
  | RequestError Text
  deriving (Show, Eq)

-- FUNCTIONS -------------------------------------------------------------------

mkRequester :: MonadRequester m => Manager -> Requester m
mkRequester = Requester . requester

request :: (ToRequest a, HasRequester r m)
        => a
        -> m (Result BSL.ByteString)
request r = do
  req    <- asks getter
  result <- unRequester req $ toRequest r
  case result of
    Left  e -> return $ RequestError $ Text.pack $ show e
    Right r -> return $ Result $ HTTP.responseBody r

decode :: forall a . FromJSON a => Result BSL.ByteString -> Result a
decode (Result bs)        = case eitherDecode bs of
  Left  e -> DecodeError (Text.pack e) $ decodeUtf8 $ BSL.toStrict bs
  Right r -> Result r
decode (RequestError e)   = RequestError e
decode (DecodeError  e t) = DecodeError  e t

requestAndDecode :: forall b a r m . (ToRequest a, FromJSON b, HasRequester r m)
                 => a
                 -> m (Result b)
requestAndDecode req = decode <$> request req
