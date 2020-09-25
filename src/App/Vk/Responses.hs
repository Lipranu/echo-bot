{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module App.Vk.Responses
  ( Attachment (..)
  , AttachmentBody (..)
  , Command (..)
  , DocumentBody (..)
  , FileSaved (..)
  , FileUploaded (..)
  , LongPollServer (..)
  , Message (..)
  , MessageSended (..)
  , Payload (..)
  , PhotoBody (..)
  , PhotoSaved (..)
  , ResponseException
  , Update (..)
  , Updates (..)
  , UploadException
  , UploadServer (..)
  , UserName (..)
  ) where

-- IMPORTS -----------------------------------------------------------------

import App.Shared.Responses

import Infrastructure.Has
import Infrastructure.Logger

import Control.Applicative ( (<|>) )
import Control.Monad.Catch ( Exception )
import Data.Aeson          ( (.:), (.:?) )
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
  } deriving (Show, Generic)

instance Exception ResponseException

instance Aeson.FromJSON ResponseException where
  parseJSON = Aeson.parseJsonDrop

instance Loggable ResponseException where
  toLog ResponseException {..}
    = mkToLog "An error occurred as a result of the request:"
    [ ("Error Code"        , Text.showt eErrorCode)
    , ("Error Message"     , eErrorMsg)
    , ("Request Parameters", params)
    ] []
    where params = Text.concat $ fmap toLog eRequestParams

instance HasPriority ResponseException where logData = logError . toLog

-- UploadException ---------------------------------------------------------

data UploadException = UploadException
  { uebError  :: Text
  , uebBwact  :: Text
  , uebServer :: Integer
  , ueb_sig   :: Text
  } deriving (Show, Generic)

instance Exception UploadException

instance Aeson.FromJSON UploadException where
  parseJSON = Aeson.parseJsonDrop

instance Loggable UploadException where
  toLog UploadException {..} = mkToLog "Error occurred during upload file:"
    [ ("Error" , uebError)
    , ("Bwact" , uebBwact)
    , ("Server", Text.showt uebServer)
    , ("_sig"  , ueb_sig)
    ] []

instance HasPriority UploadException where logData = logWarning . toLog

-- RequestParams -----------------------------------------------------------

data RequestParams = RequestParams
  { rpKey   :: Text
  , rpValue :: Text
  } deriving (Show, Generic)

instance Aeson.FromJSON RequestParams where
  parseJSON = Aeson.parseJsonDrop

instance Loggable RequestParams where
  toLog RequestParams {..} = mkLogLine ("\t" <> rpKey, rpValue)

-- LongPollServer ----------------------------------------------------------

data LongPollServer = LongPollServer
 { lpsKey    :: Text
 , lpsServer :: Text
 , lpsTs     :: Text
 } deriving Generic

instance Aeson.FromJSON LongPollServer where
  parseJSON = Aeson.parseJsonDrop

instance Loggable LongPollServer where
  toLog LongPollServer {..} = mkToLog "Recived Long Poll Server:"
    [ ("Server"   , lpsServer)
    , ("Timestamp", lpsTs)
    , ("Key"      , lpsKey)
    ] []

instance HasPriority LongPollServer where logData = logDebug . toLog

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
  toLog (Updates upds ts) = mkToLog "Resived updates:"
    [ ("Amount"       , Text.showt $ length upds)
    , ("New timestamp", ts)
    ] []

  toLog (OutOfDate ts) = mkToLog
    "Event history is outdated or partially lost:"
    [("New Timestamp", Text.showt ts)] []

  toLog KeyExpired = "Key expired"

  toLog DataLost   = "Information lost"

instance HasPriority Updates where
  logData u@(Updates _ _) = logDebug   $ toLog u
  logData u@(OutOfDate _) = logWarning $ toLog u
  logData u@KeyExpired    = logInfo    $ toLog u
  logData u@DataLost      = logWarning $ toLog u

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
  toLog (NewMessage _)   = "Resived update of type: NewMessage"
  toLog (NotSupported t) = "Not supprted update of type: " <> t

instance HasPriority Update where
  logData u@(NewMessage   _) = logInfo    $ toLog u
  logData u@(NotSupported _) = logWarning $ toLog u

-- Message -----------------------------------------------------------------

data Message = Message
  { mFromId      :: Integer
  , mPeerId      :: Integer
  , mMessage     :: Maybe Text
  , mLatitude    :: Maybe Double
  , mLongitude   :: Maybe Double
  , mReplyId     :: Maybe Integer
  , mForwardsId  :: [Integer]
  , mAttachments :: [Aeson.Value]
  , mPayload     :: Maybe Payload
  }

instance Aeson.FromJSON Message where
  parseJSON = Aeson.withObject (path <> "Message") $ \o -> do
    mFromId      <- o .:  "from_id"
    mPeerId      <- o .:  "peer_id"
    mAttachments <- o .:  "attachments"
    mPayload     <- o .:? "payload"
    mMessage     <- o .:? "text"
    mReplyId     <- o .:? "reply_message" >>= traverse (.: "id")
    mForwardsId  <- o .:  "fwd_messages"  >>= traverse (.: "id")
    mCoordinates <- o .:? "geo"           >>= traverse (.: "coordinates")
    mLatitude    <- traverse (.: "latitude")  mCoordinates
    mLongitude   <- traverse (.: "longitude") mCoordinates
    pure Message {..}

instance Loggable Message where
  toLog Message {..} = mkToLog "Message data:"
    [ ("From id"         , Text.showt mFromId)
    , ("Peer id"         , Text.showt mPeerId)
    , ("Attachments"     , Text.showt $ length mAttachments)
    , ("Forward Messages", Text.showt $ length mForwardsId)
    ]
    [ ("Message"    , mMessage)
    , ("Latitude"   , Text.showt <$> mLatitude)
    , ("Longitude"  , Text.showt <$> mLongitude)
    , ("Reply Id"   , Text.showt <$> mReplyId)
    , ("Payload"    , toLog      <$> mPayload)
    ]

instance HasPriority Message where logData = logDebug . toLog

instance Has Key Message where getter Message {..} = (mFromId, mPeerId)

instance Has (Maybe Command) Message where
  getter Message {..} = case mPayload of
    Nothing            -> Nothing
    Just (Payload _ c) -> Just c

-- Payload -----------------------------------------------------------------

data Payload = Payload Text Command

instance Aeson.FromJSON Payload where
  parseJSON = Aeson.withText (path <> "Payload") $ \t -> Payload t
    <$> Aeson.parseJSON (Aeson.String t)

instance Loggable Payload where toLog (Payload t _) = t

-- Command -----------------------------------------------------------------

data Command
  = Help
  | Repeat
  | NewCount Int
  | UnknownCommand Text

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
  toLog Help               = "Performing Help command"
  toLog Repeat             = "Performing Repeat command"
  toLog (NewCount _)       = "Setting new repeat count"
  toLog (UnknownCommand t) = "Unknown command: " <> t

instance HasPriority Command where
  logData c@(UnknownCommand _) = logWarning $ toLog c
  logData c                    = logInfo    $ toLog c

-- UserName ----------------------------------------------------------------

newtype UserName = UserName { unFirstName :: Text } deriving Generic

instance Aeson.FromJSON UserName where
  parseJSON = Aeson.parseJsonDrop

instance Loggable [UserName] where
  toLog [] = "User not found"
  toLog (x:_) = toLog x

instance HasPriority [UserName] where
  logData [] = logWarning $ toLog ([] :: [UserName])
  logData (x:_) = logData x

instance Loggable UserName where
  toLog (UserName name) = "User name: " <> name

instance HasPriority UserName where logData = logDebug . toLog

-- Attachment --------------------------------------------------------------

data Attachment
  = Attachment AttachmentBody
  | Photo PhotoBody
  | Document DocumentBody
  | Sticker Integer

instance Aeson.FromJSON Attachment where
  parseJSON = Aeson.withObject (path <> "Attachment") $ \o -> do
    aType <- o .: "type"
    case aType of
      "doc"     -> Document <$> (o .: aType >>= Aeson.parseJSON)
      "photo"   -> Photo    <$> (o .: aType >>= Aeson.parseJSON)
      "sticker" -> Sticker  <$> (o .: aType >>= (.: "sticker_id"))
      _         -> do
        body <- o .: aType >>= Aeson.parseJSON
        return $ Attachment $ body aType

instance Loggable Attachment where
  toLog (Attachment body) = toLog body
  toLog (Document   body) = toLog body
  toLog (Photo      body) = toLog body
  toLog (Sticker      id) = "Processing sticker with id: " <> Text.showt id

instance HasPriority Attachment where logData = logDebug . toLog

-- AttachmentBody ----------------------------------------------------------

data AttachmentBody = AttachmentBody
  { aId        :: Integer
  , aOwnerId   :: Integer
  , aAccessKey :: Maybe Text
  , aType      :: Text
  } deriving Generic

instance Aeson.FromJSON (Text -> AttachmentBody) where
  parseJSON = Aeson.withObject (path <> "AttachmentBody") $ \o -> do
    aId        <- o .: "id"
    aOwnerId   <- o .: "owner_id" <|> o .: "to_id"
    aAccessKey <- o .: "access_key"
    pure $ \aType -> AttachmentBody {..}

instance Loggable AttachmentBody where
  toLog AttachmentBody {..} = mkToLog "Processing AttachmentBody:"
    [ ("Type"    , aType)
    , ("Owner id", Text.showt aOwnerId)
    , ("Media id", Text.showt aId)
    ] [("Access Key", aAccessKey)]

-- PhotoBody ---------------------------------------------------------------

data PhotoBody = PhotoBody
  { pbId        :: Integer
  , pbOwnerId   :: Integer
  , pbAccessKey :: Maybe Text
  , pbUrl       :: Text
  }

instance Aeson.FromJSON PhotoBody where
  parseJSON = Aeson.withObject (path <> "PhotoBody") $ \o -> do
    pbId        <- o .:  "id"
    pbOwnerId   <- o .:  "owner_id"
    pbAccessKey <- o .:? "access_key"
    pbUrl       <- o .:  "sizes" >>= getUrl . sort
    pure PhotoBody {..}
    where getUrl xs = case (xs :: [UrlAndSize]) of
            [] -> fail $ path <> "PhotoBody: absent url"
            (UrlAndSize url _):_ -> pure url

instance Loggable PhotoBody where
  toLog PhotoBody {..} = mkToLog "Processing AttachmentBody:"
    [ ("Type"    , "photo")
    , ("Owner id", Text.showt pbOwnerId)
    , ("Media id", Text.showt pbId)
    , ("Url"     , pbUrl)
    ] [("Access Key", pbAccessKey)]

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

data Size = W | Z | Y | R | Q | P | O | X | M | S deriving (Eq, Ord)

instance Aeson.FromJSON Size where
  parseJSON = Aeson.withText (path <> "Size") $ \case
    "s" -> pure S
    "m" -> pure M
    "x" -> pure X
    "o" -> pure O
    "p" -> pure P
    "q" -> pure Q
    "r" -> pure R
    "y" -> pure Y
    "z" -> pure Z
    "w" -> pure W
    t   -> fail $ path <> ": unknown size: " <> Text.unpack t

-- DocumentBody ------------------------------------------------------------

data DocumentBody = DocumentBody
  { dUrl    :: Text
  , dTitle  :: Text
  } deriving Generic

instance Aeson.FromJSON DocumentBody where
  parseJSON = Aeson.parseJsonDrop

instance Loggable DocumentBody where
  toLog DocumentBody {..} = mkToLog "Processing DocumentBody:"
    [ ("Title", dTitle)
    , ("Url"  ,dUrl)
    ] []

-- UploadServer ------------------------------------------------------------

newtype UploadServer = UploadServer Text

instance Aeson.FromJSON UploadServer where
  parseJSON = Aeson.withObject (path <> "UploadServer") $ \o ->
    UploadServer <$> o .: "upload_url"

instance Loggable UploadServer where
  toLog (UploadServer url) = mkToLog "Recived upload server:"
    [("Url", url)] []

instance HasPriority UploadServer where logData = logDebug . toLog

-- FileUploaded ------------------------------------------------------------

data FileUploaded
  = DocumentUploaded Text
  | PhotoUploaded Integer Text Text

instance Aeson.FromJSON FileUploaded where
  parseJSON = Aeson.withObject (path <> "FileUploaded") $ \o ->
        DocumentUploaded <$> o .: "file"
    <|> PhotoUploaded    <$> o .: "server" <*> o .: "hash" <*> o .: "photo"

instance Loggable FileUploaded where
  toLog (DocumentUploaded file) = mkToLog "Document uploaded:"
    [("File", file)] []

  toLog (PhotoUploaded server hash photo) = mkToLog "Photo uploaded:"
    [ ("Server", Text.showt server)
    , ("Hash"  , hash)
    , ("Photo" , Text.showt photo)
    ] []

instance HasPriority FileUploaded where logData = logDebug . toLog

-- FileSaved ---------------------------------------------------------------

data FileSaved = FileSaved
  { fsType      :: Text
  , fsMediaId   :: Integer
  , fsOwnerId   :: Integer
  , fsAccessKey :: Maybe Text
  }

instance Aeson.FromJSON FileSaved where
  parseJSON = Aeson.withObject (path <> "FileSaved") $ \o -> do
    fsType      <- o    .:  "type"
    body        <- o    .:  fsType
    fsMediaId   <- body .:  "id"
    fsOwnerId   <- body .:  "owner_id"
    fsAccessKey <- body .:? "access_key"
    pure FileSaved {..}

instance Loggable FileSaved where
  toLog FileSaved {..} = mkToLog "File saved:"
    [ ("Type"    , fsType)
    , ("Media id", Text.showt fsMediaId)
    , ("Owner id", Text.showt fsOwnerId)
    ] [("Access Key", fsAccessKey)]

instance HasPriority FileSaved where logData = logDebug . toLog

-- PhotoSaved --------------------------------------------------------------

data PhotoSaved = PhotoSaved
  { psMediaId   :: Integer
  , psOwnerId   :: Integer
  , psAccessKey :: Maybe Text
  }

instance Aeson.FromJSON PhotoSaved where
  parseJSON = Aeson.withArray (path <> "PhotoSaved") $ \a -> case a !? 0 of
    Nothing -> fail $ path <> ": empty array"
    Just x -> (flip $ Aeson.withObject path) x $ \o -> do
      psMediaId   <- o .:  "id"
      psOwnerId   <- o .:  "owner_id"
      psAccessKey <- o .:? "access_key"
      pure PhotoSaved {..}

instance Loggable PhotoSaved where
  toLog PhotoSaved {..} = mkToLog "Photo saved:"
    [ ("Media id", Text.showt psMediaId)
    , ("Owner id", Text.showt psOwnerId)
    ] [("Access Key", psAccessKey)]

instance HasPriority PhotoSaved where logData = logDebug . toLog

-- MessageSended -----------------------------------------------------------

newtype MessageSended = MessageSended Integer
  deriving (Generic, Aeson.FromJSON)

instance Loggable MessageSended where
  toLog (MessageSended id) = "Successfully sent message with id: "
    <> Text.showt id

instance HasPriority MessageSended where logData = logInfo . toLog

-- FUNCTIONS ---------------------------------------------------------------

path :: String
path = "App.Vk.Responses."
