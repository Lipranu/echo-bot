{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications    #-}

module App.Telegram.Responses
  ( ResponseException (..)
  , Update (..)
  , Updates (..)
  ) where

-- IMPORTS --------------------------------------------------------------------

import App.Shared.Responses

import Infrastructure.Logger hiding ( Priority (..) )

import Control.Applicative ( (<|>) )
import Data.Aeson.Extended ( FromJSON (..), Value, (.:), (.:?) )
import Data.Aeson.Types    ( Parser )
import Data.Text.Extended  ( Text, showt )
import Data.Foldable       ( toList )
import Control.Monad.Catch ( Exception )
import GHC.Generics        ( Generic )

import qualified Data.Aeson.Extended as Aeson
import qualified Data.Text.Extended  as Text

-- TYPES AND INSTANCES -----------------------------------------------------

-- ResponseException -------------------------------------------------------

data ResponseException = ResponseException
  { eErrorCode          :: Integer
  , eDescription        :: Text
  , eResponseParameters :: Maybe ResponseParameters
  } deriving (Show, Generic)

instance Exception ResponseException

instance Aeson.FromJSON ResponseException where
  parseJSON = Aeson.parseJsonDrop

instance Loggable ResponseException where
  toLog ResponseException {..}
    = mkToLog "An error occurred as a result of the request:"
    [ ("Error Code"        , Text.showt eErrorCode)
    , ("Error Message"     , eDescription)
    ] [ ("ResponseParameters", toLog <$> eResponseParameters) ]

instance HasPriority ResponseException where logData = logError . toLog

-- ResponseParameters ------------------------------------------------------

data ResponseParameters
  = MigrateToChatId Integer
  | RetryAfter Integer
  deriving (Show)

instance FromJSON ResponseParameters where
  parseJSON = Aeson.withObject (path <> "ResponseParameters") $ \o ->
        MigrateToChatId <$> o .: "migrate_to_chat_id"
    <|> RetryAfter      <$> o .: "retry_after"

instance Loggable ResponseParameters where
  toLog (MigrateToChatId i) = "Migrate to chat id: " <> Text.showt i
  toLog (RetryAfter i)      = "Retry after: "        <> Text.showt i

-- Updates ------------------------------------------------------------------

newtype Updates = Updates { unUpdates :: [Value] }

instance FromJSON Updates where
  parseJSON = Aeson.withArray (path <> "Updates") $ \a ->
    pure . Updates $ toList a

instance Loggable Updates where
  toLog (Updates xs) = "Get updates: " <> showt (length xs)

instance HasPriority Updates where logData = logInfo . toLog

-- Update ------------------------------------------------------------------

data Update = Update Integer MessageType

instance FromJSON Update where
  parseJSON = Aeson.withObject (path <> "Update") $ \o -> Update
    <$> o .: "update_id"
    <*> Aeson.parseJSON (Aeson.Object o)

instance Loggable [Update] where
  toLog v = "Updates resived: " <> Text.showt (length v)

instance HasPriority [Update] where logData = logInfo . toLog

instance Loggable Update where
  toLog (Update i _) = "Proccess post with id: " <> Text.showt i

instance HasPriority Update where
  logData = logInfo . toLog

-- MessageType -------------------------------------------------------------

data MessageType
  = Message MessageData
  | UnsupportedType Aeson.Object

instance FromJSON MessageType where
  parseJSON = Aeson.withObject (path <> "MessageType") $ \o ->
        Message <$> (o .: "message" <|> o .: "channel_post")
    <|> pure (UnsupportedType o)

instance Loggable MessageType where
  toLog (Message body) = "Recived update of type Message"
  toLog (UnsupportedType body) = mkToLog "Recived unsupported update"
    [("Body", showt body)] []

instance HasPriority MessageType where
  logData m@(Message         _) = logInfo    $ toLog m
  logData m@(UnsupportedType _) = logWarning $ toLog m

-- MessageData -------------------------------------------------------------

data MessageData = MessageData
  { mdText      :: Maybe Text
  , mdMessageId :: Integer
  }

instance FromJSON MessageData where
  parseJSON = Aeson.withObject (path <> "MessageData") $ \o -> do
    mdText      <- o .: "text" <|> o .:? "caption"
    mdMessageId <- o .: "message_id"
    pure MessageData {..}

-- FUNCTIONS ---------------------------------------------------------------

path :: String
path = "App.Telegram.Responses."
