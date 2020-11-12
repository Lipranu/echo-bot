{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE UndecidableInstances       #-}

module App.Telegram.Responses
  ( MessageType (..)
  , MessageEntity (..)
  , ResponseException (..)
  , Update (..)
  , UserId (..)
  , UpdateType (..)
  , Updates (..)
  , MessageBody (..)
  , DiceBody (..)
  , PollBody (..)
  , PollOption (..)
  , ContactBody (..)
  , FileId (..)
  , VenueBody (..)
  , LocationBody (..)
  ) where

-- IMPORTS -----------------------------------------------------------------

import Infrastructure.Logger hiding ( Priority (..) )

import Control.Applicative ( (<|>) )
import Control.Monad.Catch ( Exception )
import Data.Aeson.Extended ( DropPrefix (..), FromJSON (..), (.:), (.:?) )
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
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToLayout, LogText, Exception)
    deriving FromJSON via DropPrefix ResponseException
    deriving Loggable via LogError   ResponseException

-- ResponseParameters ------------------------------------------------------

data ResponseParameters
  = MigrateToChatId Integer
  | RetryAfter Integer
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToLayout

instance FromJSON ResponseParameters where
  parseJSON = Aeson.withObject (path <> "ResponseParameters") $ \o ->
        MigrateToChatId <$> o .: "migrate_to_chat_id"
    <|> RetryAfter      <$> o .: "retry_after"

-- Updates -----------------------------------------------------------------

newtype Updates = Updates { unUpdates :: [Aeson.Value] }

instance FromJSON Updates where
  parseJSON = Aeson.withArray (path <> "Updates") $ \a ->
    pure . Updates $ toList a

instance Loggable Updates where
  logData (Updates xs) = logInfo $ "Get updates: " <> showt (length xs)

-- Update ------------------------------------------------------------------

data Update = Update Integer UpdateType

instance FromJSON Update where
  parseJSON = Aeson.withObject (path <> "Update") $ \o -> Update
    <$> o .: "update_id"
    <*> Aeson.parseJSON (Aeson.Object o)

instance Loggable [Update] where
  logData v = logInfo $ "Updates resived: " <> Text.showt (length v)

instance Loggable Update where
  logData (Update i _) = logInfo $ "Proccess post with id: " <> Text.showt i

-- UpdateType --------------------------------------------------------------

data UpdateType
  = Message MessageBody
  | UnsupportedUpdate Aeson.Object

instance FromJSON UpdateType where
  parseJSON = Aeson.withObject (path <> "MessageType") $ \o ->
        Message <$> (o .: "message" <|> o .: "channel_post")
    <|> pure (UnsupportedUpdate o)

instance Loggable UpdateType where
  logData (Message body) = logInfo "Recived update of type Message"
  logData (UnsupportedUpdate body) = logWarning $ mkLogText
    "Recived unsupported update"
    [("Body", showt body)]

-- MessageBody -------------------------------------------------------------

data MessageBody = MessageBody
  { mbMessageId :: Integer
  , mbUserId    :: Integer
  , mbChatId    :: Integer
  , mbText      :: Maybe Text
  , mbType      :: MessageType
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToLayout, LogText)
    deriving Loggable via LogDebug MessageBody

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

-- MessageType -------------------------------------------------------------

newtype FileId = FileId { getFileId :: Text }
  deriving stock (Show, Generic)
  deriving newtype Eq
  deriving anyclass ToLayout
  deriving FromJSON via DropPrefix FileId

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
  | Location  LocationBody
  | Venue     VenueBody
  | Contact   ContactBody
  | Dice      DiceBody
  | Poll      PollBody
--TODO: | MediaGroup
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToLayout, LogText)

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
    <|> Location  <$>  o .: "location"
    <|> Contact   <$>  o .: "contact"
    <|> Dice      <$>  o .: "dice"
    <|> Poll      <$>  o .: "poll"
--TODO: | MediaGroup
    <|> pure TextMessage

instance Loggable MessageType where
  logData m = case m of
    TextMessage      -> logDebug "Text Message"
    Animation   id   -> addId id "Animation"
    Audio       id   -> addId id "Audio"
    Document    id   -> addId id "Document"
    Photo       id   -> addId id "Photo"
    Sticker     id   -> addId id "Sticker"
    Video       id   -> addId id "Video"
    VideoNote   id   -> addId id "VideoNote"
    Voice       id   -> addId id "Voice"
    Location    body -> logData body
    Venue       body -> logData body
    Dice        body -> logData body
    Poll        body -> logData body
    Contact     body -> logData body
--TODO: | MediaGroup
    where addId id t = logDebug $ t <> mkLogTextLine (t <> " Id", coerce id)

-- LocationBody ------------------------------------------------------------

data LocationBody = LocationBody
  { longitude :: Double
  , latitude  :: Double
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToLayout, LogText)
    deriving Loggable via LogDebug LocationBody

-- VenueBody ---------------------------------------------------------------

data VenueBody = VenueBody
  { vbLocation       :: LocationBody
  , vbTitle          :: Text
  , vbAddress        :: Text
  , vbFoursquareId   :: Maybe Text
  , vbFoursquareType :: Maybe Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToLayout, LogText)
    deriving FromJSON via DropPrefix VenueBody
    deriving Loggable via LogDebug   VenueBody

-- ContactBody -------------------------------------------------------------

data ContactBody = ContactBody
  { cbPhoneNumber :: Text
  , cbFirstName   :: Text
  , cbLastName    :: Maybe Text
  , cbVcard       :: Maybe Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToLayout, LogText)
    deriving FromJSON via DropPrefix ContactBody
    deriving Loggable via LogDebug   ContactBody

-- DiceBody ----------------------------------------------------------------

data DiceBody = DiceBody
  { dbEmoji :: Text
  , dbValue :: Integer
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToLayout, LogText)
    deriving FromJSON via DropPrefix DiceBody
    deriving Loggable via LogDebug   DiceBody

-- PollBody ----------------------------------------------------------------

data PollBody = PollBody
  { pbId                    :: Text
  , pbQuestion              :: Text
  , pbOptions               :: [PollOption]
  , pbTotalVoterCount       :: Integer
  , pbIsClosed              :: Bool
  , pbIsAnonymous           :: Bool
  , pbType                  :: Text
  , pbAllowsMultipleAnswers :: Bool
  , pbCorrectOptionId       :: Maybe Integer
  , pbExplanation           :: Maybe Text
  , pbExplanationEntities   :: Maybe [MessageEntity]
  , pbOpenPeriod            :: Maybe Integer
  , pbCloseDate             :: Maybe Integer
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToLayout, LogText)
    deriving FromJSON via DropPrefix PollBody
    deriving Loggable via LogDebug   PollBody

-- PollOption --------------------------------------------------------------

data PollOption = PollOption
  { poText       :: Text
  , poVoterCount :: Integer
  } deriving stock (Show, Eq, Generic)
    deriving anyclass ToLayout
    deriving FromJSON via DropPrefix PollOption

-- MessageEntity -----------------------------------------------------------

newtype UserId = UserId { getUserId :: Integer }
  deriving stock (Show, Generic)
  deriving newtype Eq
  deriving anyclass ToLayout
  deriving FromJSON via DropPrefix UserId

data MessageEntity = MessageEntity
  { meType     :: Text
  , meOffset   :: Integer
  , meLength   :: Integer
  , meUrl      :: Maybe Text
  , meUser     :: Maybe UserId
  , meLanguage :: Maybe Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass ToLayout
    deriving FromJSON via DropPrefix MessageEntity

-- FUNCTIONS ---------------------------------------------------------------

path :: String
path = "App.Telegram.Responses."
