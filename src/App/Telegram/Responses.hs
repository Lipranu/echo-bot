{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
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
  , MessageBody (..)
  , FileId (..)
  , Longitude (..)
  , Latitude (..)
  , VenueBody (..)
  , LocationBody (..)
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
  | Location  Longitude Latitude
  | Venue     VenueBody
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
    <|> Venue     <$>  o .: "venue"
    <|> Location  <$>  o .: "location" <*> o .: "location"
--TODO: | MediaGroup
--TODO: | Contact
--TODO: | Dice
--TODO: | Poll
    <|> pure TextMessage

instance Loggable MessageType where
  toLog m = case m of
    TextMessage        -> "Text Message"
    Animation id       -> addId id "Animation"
    Audio     id       -> addId id "Audio"
    Document  id       -> addId id "Document"
    Photo     id       -> addId id "Photo"
    Sticker   id       -> addId id "Sticker"
    Video     id       -> addId id "Video"
    VideoNote id       -> addId id "VideoNote"
    Voice     id       -> addId id "Voice"
    Location  long lat -> "Location" <> toLog long <> toLog lat
    Venue     body     -> "Venue"    <> toLog body
--TODO: | MediaGroup
--TODO: | Contact
--TODO: | Dice
--TODO: | Poll
--TODO: | Venue
    where addId id t = t <> mkLogLine (t <> " Id", coerce id)

instance HasPriority MessageType where
  logData = logDebug . toLog

-- LocationBody ------------------------------------------------------------

data LocationBody = LocationBody
  { longitude :: Double
  , latitude  :: Double
  } deriving Generic

instance FromJSON LocationBody

instance Loggable LocationBody where
  toLog LocationBody {..} = mkToLog "Location:"
    [ (" |\tLongitude", showt longitude)
    , (" |\tLatitude", showt latitude)
    ] []

newtype Longitude = Longitude { longitude' :: Double }
  deriving Generic

instance FromJSON Longitude

instance Loggable Longitude where
  toLog (Longitude v) = mkLogLine ("Longitude", showt v)

newtype Latitude = Latitude { latitude' :: Double }
  deriving Generic

instance FromJSON Latitude

instance Loggable Latitude where
  toLog (Latitude v) = mkLogLine ("Latitude", showt v)

-- Venue -------------------------------------------------------------------

data VenueBody = VenueBody
  { vbLocation       :: LocationBody
  , vbTitle          :: Text
  , vbAddress         :: Text
  , vbFoursquareId   :: Maybe Text
  , vbFoursquareType :: Maybe Text
  } deriving Generic

instance FromJSON VenueBody where
  parseJSON = Aeson.parseJsonDrop

instance Loggable VenueBody where
  toLog VenueBody {..} = (mkToLog "Venue:"
    [ ("Title" , vbTitle)
    , ("Address", vbAddress)
    ]
    [ ("Forursquare Id" , vbFoursquareId)
    , ("Foursquare Type", vbFoursquareType)
    ]) <> toLog vbLocation

-- FUNCTIONS ---------------------------------------------------------------

path :: String
path = "App.Telegram.Responses."
