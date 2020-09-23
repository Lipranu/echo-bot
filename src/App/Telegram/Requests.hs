{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

module App.Telegram.Requests
  ( GetUpdates (..)
  ) where

-- IMPORTS --------------------------------------------------------------------

import App.Telegram.Config      ( TelegramReader, Token (..) )

import Infrastructure.Has
import Infrastructure.Logger
import Infrastructure.Requester

import Data.Aeson.Extended         ( encode )
import Data.Text.Encoding.Extended ( encodeUtf8, encodeShowUtf8 )
import Data.Text.Extended          ( Text )

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Extended   as Text
import qualified Network.HTTP.Client  as HTTP

-- TYPES AND INSTANCES -----------------------------------------------------

-- GetUpdates --------------------------------------------------------------

newtype GetUpdates = GetUpdates (Maybe Integer)

instance (TelegramReader r m, Monad m) => ToRequest m GetUpdates where
  toRequest (GetUpdates Nothing) = do
    token <- obtain
    return $
      HTTP.urlEncodedBody defaultGetUpdatesBody $
      defaultRequest
      { HTTP.path = defaultPath token <> "/getUpdates"
      , HTTP.method = "GET"
      }

  toRequest (GetUpdates (Just n)) = do
    token <- obtain
    return $
      HTTP.urlEncodedBody mkBody $
      defaultRequest
      { HTTP.path = defaultPath token <> "/getUpdates" }
    where mkBody = ("offset" , encodeShowUtf8 $ n + 1)
                 : defaultGetUpdatesBody

instance Loggable GetUpdates where
  toLog (GetUpdates Nothing)
    = "GetUpdates request without offset"
  toLog (GetUpdates (Just n))
    = "GetUpdates request with offset: " <> Text.showt (n + 1)

-- FUNCTIONS ---------------------------------------------------------------

defaultRequest :: HTTP.Request
defaultRequest = HTTP.defaultRequest
  { HTTP.host = "api.telegram.org"
  , HTTP.method = "POST"
  , HTTP.secure = True
  , HTTP.port   = 443
  }

defaultPath :: Token -> BS.ByteString
defaultPath token = "/bot" <> encodeUtf8 (unToken token)

defaultGetUpdatesBody :: [(BS.ByteString, BS.ByteString)]
defaultGetUpdatesBody =
  let list :: [Text]
      list = ["message", "edited_channel_post", "callback_query"]
   in [ ("timeout", "25")
      , ("allowed_updates", LBS.toStrict $ encode list)
      ]
