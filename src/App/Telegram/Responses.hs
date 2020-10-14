{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module App.Telegram.Responses
  ( MessageType (..)
  , ResponseException (..)
  , Update (..)
  , UpdateType (..)
  , Updates (..)
  ) where

-- IMPORTS -----------------------------------------------------------------

import App.Shared.Responses

import Infrastructure.Logger hiding ( Priority (..) )

import Control.Applicative ( (<|>) )
import Control.Monad.Catch ( Exception )
import Data.Aeson.Extended ( FromJSON (..), Value, (.:), (.:?) )
import Data.Coerce         ( coerce )
import Data.Foldable       ( toList )
import Data.Text.Extended  ( Text, showt )
import Data.Vector         ( (!) )
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
  deriving Show

instance FromJSON ResponseParameters where
  parseJSON = Aeson.withObject (path <> "ResponseParameters") $ \o ->
        MigrateToChatId <$> o .: "migrate_to_chat_id"
    <|> RetryAfter      <$> o .: "retry_after"

instance Loggable ResponseParameters where
  toLog (MigrateToChatId i) = "Migrate to chat id: " <> Text.showt i
  toLog (RetryAfter i)      = "Retry after: "        <> Text.showt i

-- Updates -----------------------------------------------------------------

newtype Updates = Updates { unUpdates :: [Value] }

instance FromJSON Updates where
  parseJSON = Aeson.withArray (path <> "Updates") $ \a ->
    pure . Updates $ toList a

instance Loggable Updates where
  toLog (Updates xs) = "Get updates: " <> showt (length xs)

instance HasPriority Updates where logData = logInfo . toLog

-- Update ------------------------------------------------------------------

data Update = Update Integer UpdateType

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

-- UpdateType --------------------------------------------------------------

data UpdateType
  = Message MessageBody
  | UnsupportedUpdate Aeson.Object

instance FromJSON UpdateType where
  parseJSON = Aeson.withObject (path <> "MessageType") $ \o ->
        Message <$> (o .: "message" <|> o .: "channel_post")
    <|> pure (UnsupportedUpdate o)

instance Loggable UpdateType where
  toLog (Message body) = "Recived update of type Message"
  toLog (UnsupportedUpdate body) = mkToLog "Recived unsupported update"
    [("Body", showt body)] []

instance HasPriority UpdateType where
  logData m@(Message           _) = logInfo    $ toLog m
  logData m@(UnsupportedUpdate _) = logWarning $ toLog m

-- MessageBody -------------------------------------------------------------

data MessageBody = MessageBody
  { mbMessageId :: Integer
  , mbUserId    :: Integer
  , mbChatId    :: Integer
  , mbText      :: Maybe Text
  , mbType      :: MessageType
  }

instance FromJSON MessageBody where
  parseJSON = Aeson.withObject (path <> "MessageData") $ \o -> do
    mbType      <- parseJSON $ Aeson.Object o
    mbMessageId <- o .:  "message_id"
    mbText      <- o .:  "text" <|> o .:? "caption"
    mbChatId    <- o .:  "chat" >>= (.: "id")
    mbUserId    <- o .:? "from" >>= \case
      Nothing   -> pure mbChatId
      Just user -> user .: "id"
    pure MessageBody {..}

instance Loggable MessageBody where
  toLog MessageBody {..} = mkToLog "Processing MessageBody:"
    [ ("Message Type", toLog mbType)
    , ("Message Id"  , showt mbMessageId)
    , ("User Id"     , showt mbUserId)
    , ("Chat Id"     , showt mbChatId)
    ] [("Text", mbText)]

instance HasPriority MessageBody where
  logData = logDebug . toLog

-- MessageType -------------------------------------------------------------

newtype FileId = FileId { unFileId :: Text } deriving Generic

instance FromJSON FileId where
  parseJSON = Aeson.parseJsonDrop

data MessageType
  = TextMessage
  | Animation FileId
  | Audio     FileId
  | Document  FileId
  | Photo     FileId
  | Sticker   FileId
  | Video     FileId
  | VideoNote FileId
  | Voice     FileId
--TODO: | Location
--TODO: | MediaGroup
--TODO: | Contact
--TODO: | Dice
--TODO: | Poll
--TODO: | Venue

instance FromJSON MessageType where
  parseJSON = Aeson.withObject (path <> "Attachment") $ \o ->
        Photo     <$> (o .: "photo" >>= parseJSON . (! 0))
    <|> Animation <$>  o .: "animation"
    <|> Audio     <$>  o .: "audio"
    <|> Document  <$>  o .: "document"
    <|> Sticker   <$>  o .: "sticker"
    <|> Video     <$>  o .: "video"
    <|> VideoNote <$>  o .: "video_note"
    <|> Voice     <$>  o .: "voice"
    <|> pure TextMessage

instance Loggable MessageType where
  toLog TextMessage    = "Text Message"
  toLog (Animation id) = "Animation (id: " <> coerce id <> ")"
  toLog (Audio id)     = "Audio (id: "     <> coerce id <> ")"
  toLog (Document id)  = "Document (id: "  <> coerce id <> ")"
  toLog (Photo id)     = "Photo (id: "     <> coerce id <> ")"
  toLog (Sticker id)   = "Sticker (id: "   <> coerce id <> ")"
  toLog (Video id)     = "Video (id:"      <> coerce id <> ")"
  toLog (VideoNote id) = "VideoNote (id: " <> coerce id <> ")"
  toLog (Voice id)     = "Voice (id: "     <> coerce id <> ")"

instance HasPriority MessageType where
  logData = logDebug . toLog

-- FUNCTIONS ---------------------------------------------------------------

path :: String
path = "App.Telegram.Responses."
