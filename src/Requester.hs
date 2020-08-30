--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}

module Requester
  ( MonadRequester (..)
  , Requester

  , mkRequester
  ) where

-- IMPORTS ---------------------------------------------------------------------


--import qualified Data.Aeson as Aeson
import Control.Monad.Reader         ( MonadReader, asks )
import Data.Text (Text)
import Network.HTTP.Client.Extended ( Request
                                    , Response
                                    , Manager
                                    , HttpException
                                    )

import qualified Data.ByteString.Lazy as BSL
import qualified Network.HTTP.Client.Extended as HTTP
import qualified Data.Text as Text

-- CLASSES ---------------------------------------------------------------------

class Monad m => MonadRequester m where
  requester :: Manager
            -> Request
            -> m (Either HttpException (Response BSL.ByteString))

class ToRequest a where
  toRequest :: a -> Request

-- TYPES AND INSTANCES ---------------------------------------------------------

newtype Requester m = Requester
  { unRequester :: Request
                -> m (Either HttpException (Response BSL.ByteString))
  }

-- FUNCTIONS -------------------------------------------------------------------

mkRequester :: MonadRequester m => Manager -> Requester m
mkRequester = Requester . requester
