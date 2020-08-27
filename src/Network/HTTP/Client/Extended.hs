module Network.HTTP.Client.Extended
  ( module Network.HTTP.Client
  , defaultRequest
  ) where

import           Network.HTTP.Client hiding ( defaultRequest )
import qualified Network.HTTP.Client     as HTTP

defaultRequest :: Request
defaultRequest = HTTP.defaultRequest
  { secure = True
  , port   = 443
  }
