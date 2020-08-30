--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}

module Requester
  ( MonadRequester (..)
  ) where

-- IMPORTS ---------------------------------------------------------------------

import qualified Network.HTTP.Client.Extended as HTTP
--import Network.HTTP.Client.TLS ( tlsManagerSettings )
--import qualified Data.Aeson as Aeson
--import Control.Exception (try)
--import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as BSL
--import Data.Text (Text)
--import           Data.Text.Encoding  (decodeUtf8)
--import qualified Data.Text as Text

-- CLASSES ---------------------------------------------------------------------

class Monad m => MonadRequester m where
  request :: HTTP.Request -> HTTP.Manager -> m (HTTP.Response BSL.ByteString)

-- TYPES AND INSTANCES ---------------------------------------------------------



-- FUNCTIONS -------------------------------------------------------------------

