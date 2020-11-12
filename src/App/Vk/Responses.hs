{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE UndecidableInstances  #-}

module App.Vk.Responses
  ( Attachment (..)
  , AttachmentBody (..)
  , AudioMessageBody (..)
  , Command (..)
  , Context (..)
  , DocumentBody (..)
  , FileSaved (..)
  , FileUploaded (..)
  , FromId (..)
  , LongPollServer (..)
  , Message (..)
  , MessageId (..)
  , MessageSended (..)
  , Payload (..)
  , PeerId (..)
  , PhotoBody (..)
  , PhotoSaved (..)
  , ResponseException
  , Update (..)
  , Updates (..)
  , UploadException
  , UploadServer (..)
  , UserName (..)
  , VideoBody (..)
  ) where

-- IMPORTS -----------------------------------------------------------------

import App.Shared.Responses

import Infrastructure.Has
import Infrastructure.Logger

import Control.Applicative ( (<|>) )
import Control.Monad       ( join )
import Control.Monad.Catch ( Exception )
import Data.Aeson.Extended ( DropPrefix (..), FromJSON (..), (.:), (.:?) )
import Data.List           ( sort )
import Data.Text.Extended  ( Text )
import Data.Vector         ( (!?) )
import GHC.Generics        ( Generic )

import qualified Data.Aeson.Extended as Aeson
import qualified Data.Text.Extended  as Text

-- TYPES AND INSTANCES -----------------------------------------------------

-- ResponseException -------------------------------------------------------

data ResponseException = ResponseException
  { eErrorCode     :: Integer
  , eErrorMsg      :: Text
  , eRequestParams :: [RequestParams]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToLayout, LogText, Exception)
    deriving FromJSON via DropPrefix ResponseException
    deriving Loggable via LogError   ResponseException

-- UploadException ---------------------------------------------------------

data UploadException = UploadException
  { uebError  :: Text
  , uebBwact  :: Text
  , uebServer :: Integer
  , ueb_sig   :: Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToLayout, LogText, Exception)
    deriving FromJSON via DropPrefix UploadException
    deriving Loggable via LogError   UploadException

-- RequestParams -----------------------------------------------------------

data RequestParams = RequestParams
  { rpKey   :: Text
  , rpValue :: Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass ToLayout
    deriving FromJSON via DropPrefix RequestParams

-- LongPollServer ----------------------------------------------------------

data LongPollServer = LongPollServer
  { lpsKey    :: Text
  , lpsServer :: Text
  , lpsTs     :: Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToLayout, LogText)
    deriving FromJSON via DropPrefix LongPollServer
    deriving Loggable via LogDebug   LongPollServer

-- Updates -----------------------------------------------------------------

data Updates
  = Updates [Aeson.Value] Text
  | OutOfDate Integer
  | KeyExpired
  | DataLost

instance Aeson.FromJSON Updates where
  parseJSON = Aeson.withObject (path <> "Updates") $ \o -> do
    result <- o .:? "updates"
    case result of
      Just updates -> Updates updates <$> o .: "ts"
      Nothing -> do
        code <- o .: "failed"
        case code :: Integer of
          1 -> OutOfDate <$> o .: "ts"
          2 -> pure KeyExpired
          3 -> pure DataLost
          e -> fail $ "App.Vk.Responses.Updates: Unknown error key: "
            <> show e

instance Loggable Updates where
  logData (Updates upds ts) = logDebug $ mkLogText "Resived updates:"
    [ ("Amount"       , Text.showt $ length upds)
    , ("New timestamp", ts)
    ]

  logData (OutOfDate ts) = logWarning $ mkLogText
    "Event history is outdated or partially lost:"
    [("New Timestamp", Text.showt ts)]

  logData KeyExpired = logInfo "Key expired"

  logData DataLost = logWarning "Information lost"

-- Update ------------------------------------------------------------------

data Update
  = NewMessage Message
  | NotSupported Text

instance Aeson.FromJSON Update where
  parseJSON = Aeson.withObject (path <> "Update") $ \o -> do
    t <- o .: "type"
    case t of
      "message_new" -> NewMessage <$> (o .: "object" >>= (.: "message"))
      _ -> return $ NotSupported t

instance Loggable Update where
  logData (NewMessage m)   = logInfo "Resived update of type: NewMessage"
    >> logData m
  logData (NotSupported t) = logWarning
    $ "Not supprted update of type: " <> t

-- Message -----------------------------------------------------------------

newtype MessageId = MessageId { getMessageId :: Integer }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToLayout

newtype FromId = FromId { getFromId :: Integer }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToLayout

newtype PeerId = PeerId { getPeerId :: Integer }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToLayout

data Message = Message
  { mId          :: MessageId
  , mFromId      :: FromId
  , mPeerId      :: PeerId
  , mMessage     :: Maybe Text
  , mLatitude    :: Maybe Double
  , mLongitude   :: Maybe Double
  , mSticker     :: Maybe Integer
  , mReplyId     :: Maybe Integer
  , mForwardsId  :: [Integer]
  , mAttachments :: [Aeson.Value]
  , mPayload     :: Maybe Payload
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToLayout, LogText)
    deriving Loggable via LogDebug Message

instance Aeson.FromJSON Message where
  parseJSON = Aeson.withObject (path <> "Message") $ \o -> do
    let mSticker  = Nothing
    mId          <- MessageId <$> o .: "id"
    mFromId      <- FromId    <$> o .: "from_id"
    mPeerId      <- PeerId    <$> o .: "peer_id"
    mAttachments <- o .:  "attachments"
    mPayload     <- o .:? "payload"
    mMessage     <- o .:? "text"
    mReplyId     <- o .:? "reply_message" >>= traverse (.: "id")
    mForwardsId  <- o .:  "fwd_messages"  >>= traverse (.: "id")
    mCoordinates <- o .:? "geo"           >>= traverse (.: "coordinates")
    mLatitude    <- traverse (.: "latitude")  mCoordinates
    mLongitude   <- traverse (.: "longitude") mCoordinates
    pure Message {..}

instance Has MessageId     Message where getter = mId
instance Has PeerId        Message where getter = mPeerId
instance Has FromId        Message where getter = mFromId
instance Has [Aeson.Value] Message where getter = mAttachments

instance Has Context Message where
  getter Message {..} | getFromId mFromId == getPeerId mPeerId = Private
                      | otherwise                              = Chat

instance Has Key Message where
  getter Message {..} = (getFromId mFromId, getPeerId mPeerId)

instance Has (Maybe Command) Message where
  getter Message {..} = case mPayload of
    Nothing            -> Nothing
    Just (Payload _ c) -> Just c

-- Context -----------------------------------------------------------------

data Context
  = Private
  | Chat
  deriving stock (Show, Eq)

-- Payload -----------------------------------------------------------------

data Payload = Payload Text Command
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToLayout

instance Aeson.FromJSON Payload where
  parseJSON = Aeson.withText (path <> "Payload") $ \t -> Payload t
    <$> Aeson.parseJSON (Aeson.String t)

-- Command -----------------------------------------------------------------

data Command
  = Help
  | Repeat
  | NewCount Int
  | UnknownCommand Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToLayout

instance Aeson.FromJSON Command where
  parseJSON = Aeson.withText (path <> "Command") $ \case
    "101" -> pure Help
    "102" -> pure Repeat
    "201" -> pure $ NewCount 1
    "202" -> pure $ NewCount 2
    "203" -> pure $ NewCount 3
    "204" -> pure $ NewCount 4
    "205" -> pure $ NewCount 5
    text  -> pure $ UnknownCommand text

instance Loggable Command where
  logData Help               = logDebug "Performing Help command"
  logData Repeat             = logDebug "Performing Repeat command"
  logData (NewCount _)       = logDebug "Setting new repeat count"
  logData (UnknownCommand t) = logWarning $ "Unknown command: " <> t

-- UserName ----------------------------------------------------------------

newtype UserName = UserName { getFirstName :: Text }
  deriving stock Generic
  deriving FromJSON via DropPrefix UserName

instance Loggable [UserName] where
  logData [] = logWarning "User not found"
  logData (x:_) = logData x

instance Loggable UserName where
  logData (UserName name) = logDebug $ "User name: " <> name

-- Attachment --------------------------------------------------------------

data Attachment
  = Attachment AttachmentBody
  | Photo PhotoBody
  | Video VideoBody
  | Document DocumentBody
  | AudioMessage AudioMessageBody
  | Graffiti
  | Sticker Integer

instance Aeson.FromJSON Attachment where
  parseJSON = Aeson.withObject (path <> "Attachment") $ \o -> do
    aType <- o .: "type"
    case aType of
      "graffiti"      -> pure Graffiti
      "doc"           -> Document     <$> (o .: aType >>= Aeson.parseJSON)
      "photo"         -> Photo        <$> (o .: aType >>= Aeson.parseJSON)
      "video"         -> Video        <$> (o .: aType >>= Aeson.parseJSON)
      "audio_message" -> AudioMessage <$> (o .: aType >>= Aeson.parseJSON)
      "sticker"       -> Sticker      <$> (o .: aType >>= (.: "sticker_id"))
      _               -> do
        body <- o .: aType >>= Aeson.parseJSON
        pure $ Attachment $ body aType

instance Loggable Attachment where
  logData Graffiti            = logDebug "Processing graffiti"
  logData (Attachment   body) = logData body
  logData (Document     body) = logData body
  logData (AudioMessage body) = logData body
  logData (Photo        body) = logData body
  logData (Video        body) = logData body
  logData (Sticker        id) = logDebug $ "Processing sticker with id: "
    <> Text.showt id

-- AttachmentBody ----------------------------------------------------------

data AttachmentBody = AttachmentBody
  { aId        :: Integer
  , aOwnerId   :: Integer
  , aAccessKey :: Maybe Text
  , aType      :: Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToLayout, LogText)
    deriving Loggable via LogDebug AttachmentBody

instance Aeson.FromJSON (Text -> AttachmentBody) where
  parseJSON = Aeson.withObject (path <> "AttachmentBody") $ \o -> do
    aId        <- o .:  "id"
    aOwnerId   <- o .:  "owner_id" <|> o .: "to_id"
    aAccessKey <- o .:? "access_key"
    pure $ \aType -> AttachmentBody {..}

-- PhotoBody ---------------------------------------------------------------

data PhotoBody = PhotoBody
  { pbId        :: Integer
  , pbOwnerId   :: Integer
  , pbAccessKey :: Maybe Text
  , pbUrl       :: Text
  , pbTitle     :: Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToLayout, LogText)
    deriving Loggable via LogDebug PhotoBody

instance Aeson.FromJSON PhotoBody where
  parseJSON = Aeson.withObject (path <> "PhotoBody") $ \o -> do
    pbId        <- o .:  "id"
    pbOwnerId   <- o .:  "owner_id"
    pbAccessKey <- o .:? "access_key"
    pbUrl       <- o .:  "sizes" >>= getUrl . sort
    let pbTitle = urlToTitle pbUrl
    pure PhotoBody {..}
    where getUrl xs = case (xs :: [UrlAndSize]) of
            [] -> fail $ path <> "PhotoBody: absent url"
            (UrlAndSize url _):_ -> pure url

-- UrlAndSize --------------------------------------------------------------

data UrlAndSize = UrlAndSize Text Size

instance Eq UrlAndSize where
  (UrlAndSize _ x) == (UrlAndSize _ y) = x == y

instance Ord UrlAndSize where
  compare (UrlAndSize _ x) (UrlAndSize _ y) = compare x y

instance Aeson.FromJSON UrlAndSize where
  parseJSON = Aeson.withObject (path <> "UrlAndSize") $ \o -> UrlAndSize
    <$> o .: "url"
    <*> o .: "type"

-- Size --------------------------------------------------------------------

data Size = W | Z | Y | R | Q | P | O | X | M | S
  deriving stock (Generic, Eq, Ord)
  deriving anyclass FromJSON --via DropPrefix Size

-- VideoBody ---------------------------------------------------------------

data VideoBody = VideoBody
  { vbId        :: Integer
  , vbOwnerId   :: Integer
  , vbAccessKey :: Maybe Text
  , vbCanResend :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToLayout, LogText)
    deriving Loggable via LogDebug VideoBody

instance Aeson.FromJSON VideoBody where
  parseJSON = Aeson.withObject (path <> "VideoBody") $ \o -> do
    vbId        <- o .:  "id"
    vbOwnerId   <- o .:  "owner_id"
    vbAccessKey <- o .:? "access_key"
    vbCanAdd    <- o .:  "can_add"
    let vbCanResend = case vbCanAdd :: Int of
          0 -> False
          1 -> True
    pure VideoBody {..}

-- AudioMessageBody --------------------------------------------------------

data AudioMessageBody = AudioMessageBody
  { ambId        :: Integer
  , ambOwnerId   :: Integer
  , ambAccessKey :: Maybe Text
  , ambUrl       :: Text
  , ambTitle     :: Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToLayout, LogText)
    deriving Loggable via LogDebug AudioMessageBody

instance Aeson.FromJSON AudioMessageBody where
  parseJSON = Aeson.withObject (path <> "AudioMessageBody") $ \o -> do
    ambId        <- o .:  "id"
    ambOwnerId   <- o .:  "owner_id"
    ambUrl       <- o .:  "link_ogg" <|> o .: "link_mp3"
    ambAccessKey <- o .:? "access_key"
    let ambTitle = urlToTitle ambUrl
    pure AudioMessageBody {..}

-- DocumentBody ------------------------------------------------------------

data DocumentBody = DocumentBody
  { dbUrl       :: Text
  , dbTitle     :: Text
  , dbId        :: Integer
  , dbOwnerId   :: Integer
  , dbType      :: Text
  , dbAccessKey :: Maybe Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToLayout, LogText)
    deriving Loggable via LogDebug DocumentBody

instance Aeson.FromJSON DocumentBody where
  parseJSON = Aeson.withObject (path <> "DocumentBody") $ \o -> do
    dbUrl       <- o .:  "url"
    dbTitle     <- o .:  "title"
    dbId        <- o .:  "id"
    dbOwnerId   <- o .:  "owner_id"
    dbTypeInt   <- o .:  "type"
    dbAccessKey <- o .:? "access_key"
    dbPreview   <- o .:? "preview"
    dbPreviewG  <- traverse (.:? "graffiti") dbPreview
    dbPreviewA  <- traverse (.:? "audio_message") dbPreview
    let dbType = case join dbPreviewG :: Maybe Aeson.Object of
          Just _ -> "graffiti"
          _      -> case join dbPreviewA :: Maybe Aeson.Object of
            Just _ -> "audio_message"
            _      -> case dbTypeInt :: Integer of
              3 -> "photo"
              4 -> "photo"
              _ -> "doc"
    pure DocumentBody {..}

-- UploadServer ------------------------------------------------------------

newtype UploadServer = UploadServer Text

instance Aeson.FromJSON UploadServer where
  parseJSON = Aeson.withObject (path <> "UploadServer") $ \o ->
    UploadServer <$> o .: "upload_url"

instance Loggable UploadServer where
  logData (UploadServer url) = logDebug $ mkLogText "Recived upload server:"
    [("Url", url)]

-- FileUploaded ------------------------------------------------------------

data FileUploaded
  = DocumentUploaded Text
  | PhotoUploaded Integer Text Text

instance Aeson.FromJSON FileUploaded where
  parseJSON = Aeson.withObject (path <> "FileUploaded") $ \o ->
        DocumentUploaded <$> o .: "file"
    <|> PhotoUploaded    <$> o .: "server" <*> o .: "hash" <*> o .: "photo"

instance Loggable FileUploaded where
  logData (DocumentUploaded file) = logDebug $ mkLogText
    "Document uploaded:"
    [("File", file)]

  logData (PhotoUploaded server hash photo) = logInfo $ mkLogText
    "Photo uploaded:"
    [ ("Server", Text.showt server)
    , ("Hash"  , hash)
    , ("Photo" , Text.showt photo)
    ]

-- FileSaved ---------------------------------------------------------------

data FileSaved = FileSaved
  { fsType      :: Text
  , fsMediaId   :: Integer
  , fsOwnerId   :: Integer
  , fsAccessKey :: Maybe Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToLayout, LogText)
    deriving Loggable via LogDebug FileSaved

instance Aeson.FromJSON FileSaved where
  parseJSON = Aeson.withObject (path <> "FileSaved") $ \o -> do
    fsType      <- o    .:  "type"
    body        <- o    .:  fsType
    fsMediaId   <- body .:  "id"
    fsOwnerId   <- body .:  "owner_id"
    fsAccessKey <- body .:? "access_key"
    pure FileSaved {..}

-- PhotoSaved --------------------------------------------------------------

data PhotoSaved = PhotoSaved
  { psMediaId   :: Integer
  , psOwnerId   :: Integer
  , psAccessKey :: Maybe Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToLayout, LogText)
    deriving Loggable via LogDebug PhotoSaved

instance Aeson.FromJSON PhotoSaved where
  parseJSON = Aeson.withArray (path <> "PhotoSaved") $ \a -> case a !? 0 of
    Nothing -> fail $ path <> ": empty array"
    Just x -> (flip $ Aeson.withObject path) x $ \o -> do
      psMediaId   <- o .:  "id"
      psOwnerId   <- o .:  "owner_id"
      psAccessKey <- o .:? "access_key"
      pure PhotoSaved {..}

-- MessageSended -----------------------------------------------------------

newtype MessageSended = MessageSended Integer
  deriving stock Generic
  deriving anyclass FromJSON

instance Loggable MessageSended where
  logData (MessageSended id) = logInfo $
    "Successfully sent message with id: " <> Text.showt id

-- FUNCTIONS ---------------------------------------------------------------

urlToTitle :: Text -> Text
urlToTitle = snd . Text.breakOnEnd "/"

path :: String
path = "App.Vk.Responses."
