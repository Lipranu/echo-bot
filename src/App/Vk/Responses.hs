{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving     #-}

module App.Vk.Responses
  ( Attachment (..)
  , AttachmentBody (..)
  , DocumentBody (..)
  , FileSaved (..)
  , FileUploaded (..)
  , LongPollServer (..)
  , Message (..)
  , RawFile (..)
  , Response (..)
  , Update (..)
  , Updates (..)
  , UploadServer (..)
  , WallBody (..)
  , MessageSended (..)
  ) where

-- IMPORTS -----------------------------------------------------------------

import Infrastructure.Logger ( Loggable (..), HasPriority (..)
                             , logDebug, logWarning, logInfo
                             , mkToLog, mkLogLine
                             )

import Control.Applicative  ( (<|>) )
import Data.Aeson           ( (.:), (.:?) )
import Data.Text.Extended   ( Text )
import GHC.Generics         ( Generic )
import Data.ByteString.Lazy ( ByteString )

import qualified Data.Aeson.Extended as Aeson
import qualified Data.Text.Extended  as Text

-- TYPES AND INSTANCES -----------------------------------------------------

-- Response ----------------------------------------------------------------

data Response a
  = Success a
  | Error ErrorBody
  | UploadError UploadErrorBody
  deriving Functor

instance Aeson.FromJSON a => Aeson.FromJSON (Response a) where
  parseJSON = Aeson.withObject "App.Vk.Response" $ \o ->
        Error       <$> o .: "error"
    <|> UploadError <$> Aeson.parseJSON (Aeson.Object o)
    <|> Success     <$> o .: "response"
    <|> Success     <$> Aeson.parseJSON (Aeson.Object o)

instance Loggable a => Loggable (Response a) where
  toLog (Success     x) = toLog x
  toLog (Error       x) = toLog x
  toLog (UploadError x) = toLog x

instance HasPriority a => HasPriority (Response a) where
  logData (Success     x) = logData x
  logData (Error       x) = logData x
  logData (UploadError x) = logData x

-- ErrorBody ---------------------------------------------------------------

data ErrorBody = ErrorBody
  { eErrorCode     :: Integer
  , eErrorMsg      :: Text
  , eRequestParams :: [RequestParams]
  } deriving Generic

instance Aeson.FromJSON ErrorBody where
  parseJSON = Aeson.parseJsonDrop

instance Loggable ErrorBody where
  toLog ErrorBody {..}
    = mkToLog "An error occurred as a result of the request:"
    [ ("Error Code"        , Text.showt eErrorCode)
    , ("Error Message"     , eErrorMsg)
    , ("Request Parameters", params)
    ] []
    where params = Text.concat $ fmap toLog eRequestParams

instance HasPriority ErrorBody where logData = logWarning . toLog

-- UploadErrorBody ---------------------------------------------------------

data UploadErrorBody = UploadErrorBody
  { uebError  :: Text
  , uebBwact  :: Text
  , uebServer :: Integer
  , ueb_sig   :: Text
  } deriving Generic

instance Aeson.FromJSON UploadErrorBody where
  parseJSON = Aeson.parseJsonDrop

instance Loggable UploadErrorBody where
  toLog UploadErrorBody {..} = mkToLog "Error occurred during upload file:"
    [ ("Error" , uebError)
    , ("Bwact" , uebBwact)
    , ("Server", Text.showt uebServer)
    , ("_sig"  , ueb_sig)
    ] []

instance HasPriority UploadErrorBody where logData = logWarning . toLog

-- RequestParams -----------------------------------------------------------

data RequestParams = RequestParams
  { rpKey   :: Text
  , rpValue :: Text
  } deriving Generic

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
  parseJSON = Aeson.withObject "App.Vk.Updates" $ \o -> do
    result <- o .:? "updates"
    case result of
      Just updates -> do
        ts <- o .: "ts"
        return $ Updates updates ts
      Nothing -> do
        code <- o .: "failed"
        case code :: Integer of
          1 -> OutOfDate <$> o .: "ts"
          2 -> return KeyExpired
          3 -> return DataLost
          e -> fail $ "App.Vk.Updates: Unknown error key: " <> show e

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
  logData KeyExpired      = logDebug   $ toLog KeyExpired
  logData DataLost        = logWarning $ toLog DataLost

-- Update ------------------------------------------------------------------

data Update
  = NewMessage Message
  | NotSupported Text

instance Aeson.FromJSON Update where
  parseJSON = Aeson.withObject "App.Vk.Update" $ \o -> do
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
  , mAttachments :: [Aeson.Value]
--  , mKeyboard    :: Maybe Aeson.Object
  }

instance Aeson.FromJSON Message where
  parseJSON = Aeson.withObject "App.Vk.Message" $ \o -> Message
    <$> o .:  "from_id"
    <*> o .:  "peer_id"
    <*> o .:? "text"
    <*> (coord o "latitude"  <|> pure Nothing)
    <*> (coord o "longitude" <|> pure Nothing)
    <*> o .: "attachments"
    where coord o t = o .: "geo" >>= (.: "coordinates") >>= (.: t)

instance Loggable Message where
  toLog Message {..} = mkToLog "Message data:"
    [ ("From id"    , Text.showt mFromId)
    , ("Peer id"    , Text.showt mPeerId)
    , ("Attachments", Text.showt $ length mAttachments)
    ]
    [ ("Message"    , mMessage)
    , ("Latitude"   , Text.showt <$> mLatitude)
    , ("Longitude"  , Text.showt <$> mLongitude)
    ]

instance HasPriority Message where logData = logDebug . toLog

-- Attachment --------------------------------------------------------------

data Attachment
  = Attachment AttachmentBody
  | Document DocumentBody
  | Sticker Integer
  | Wall WallBody

instance Aeson.FromJSON Attachment where
  parseJSON = Aeson.withObject "App.Vk.Attachment" $ \o -> do
    aType      <- o .: "type"
    case aType of
      "doc"     -> Document <$> (o .: aType >>= Aeson.parseJSON)
      "sticker" -> Sticker  <$> (o .: aType >>= (.: "sticker_id"))
      "wall"    -> do
        body <- o .: aType >>= Aeson.parseJSON
        return $ Wall $ body aType
      _         -> do
        body <- o .: aType >>= Aeson.parseJSON
        return $ Attachment $ body aType

instance Loggable Attachment where
  toLog (Attachment _) = "Processing attachment of type: Attachment"
  toLog (Document   _) = "Processing attachment of type: Document"
  toLog (Sticker    _) = "Processing attachment of type: Sticker"
  toLog (Wall       _) = "Processing attachment of type: Wall"

instance HasPriority Attachment where logData = logDebug . toLog

-- AttachmentBody ----------------------------------------------------------

data AttachmentBody = AttachmentBody
  { aId        :: Integer
  , aOwnerId   :: Integer
  , aAccessKey :: Maybe Text
  , aType      :: Text
  } deriving Generic

instance Aeson.FromJSON (Text -> AttachmentBody) where
  parseJSON = Aeson.withObject "App.Vk.Responses.AttachmenBody"
    $ \o -> AttachmentBody
    <$> o .:  "id"
    <*> o .:  "owner_id"
    <*> o .:? "access_key"

instance Loggable AttachmentBody where
  toLog AttachmentBody {..} = mkToLog "Processing AttachmentBody:"
    [ ("Type"    , aType)
    , ("Owner id", Text.showt aOwnerId)
    , ("Media id", Text.showt aId)
    ] [("Access Key", aAccessKey)]

instance HasPriority AttachmentBody where logData = logDebug . toLog

-- WallBody ----------------------------------------------------------------

data WallBody = WallBody
  { wId        :: Integer
  , wToId   :: Integer
  , wAccessKey :: Maybe Text
  , wType      :: Text
  } deriving Generic

instance Aeson.FromJSON (Text -> WallBody) where
  parseJSON = Aeson.withObject "App.Vk.Responses.WallBody"
    $ \o -> WallBody
    <$> o .:  "id"
    <*> o .:  "to_id"
    <*> o .:? "access_key"

instance Loggable WallBody where
  toLog WallBody {..} = mkToLog "Processing WallBody:"
    [ ("Type"    , wType)
    , ("To id"   , Text.showt wToId)
    , ("Media id", Text.showt wId)
    ] [("Access Key", wAccessKey)]

instance HasPriority WallBody where logData = logDebug . toLog

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

instance HasPriority DocumentBody where logData = logDebug . toLog

-- UploadServer ------------------------------------------------------------

newtype UploadServer = UploadServer Text

instance Aeson.FromJSON UploadServer where
  parseJSON = Aeson.withObject "App.Vk.UploadServer" $ \o ->
    UploadServer <$> o .: "upload_url"

instance Loggable UploadServer where
  toLog (UploadServer url) = mkToLog "Recived upload server:"
    [("Url", url)] []

instance HasPriority UploadServer where logData = logDebug . toLog

-- RawFile -----------------------------------------------------------------

newtype RawFile = RawFile ByteString

instance Loggable RawFile where
  toLog _ = "File downloaded successfully"

instance HasPriority RawFile where logData = logDebug . toLog

-- FileUploaded ------------------------------------------------------------

newtype FileUploaded = FileUploaded Text

instance Aeson.FromJSON FileUploaded where
  parseJSON = Aeson.withObject "App.Vk.FileUploaded" $ \o ->
    FileUploaded <$> o .: "file"

instance Loggable FileUploaded where
  toLog (FileUploaded text) = mkToLog "File uploaded:"
    [("Response body", text)] []

instance HasPriority FileUploaded where logData = logDebug . toLog

-- FileSaved ---------------------------------------------------------------

data FileSaved = FileSaved
  { fsType :: Text
  , fsMediaId :: Integer
  , fsOwnerId :: Integer
  }

instance Aeson.FromJSON FileSaved where
  parseJSON = Aeson.withObject "App.Vk.FileSaved" $ \o -> do
    fsType    <- o .: "type"
    fsMediaId <- o .: fsType >>= (.: "id")
    fsOwnerId <- o .: fsType >>= (.: "owner_id")
    return FileSaved {..}

instance Loggable FileSaved where
  toLog FileSaved {..} = mkToLog "File saved:"
    [ ("Type"    , fsType)
    , ("Media id", Text.showt fsMediaId)
    , ("Owner id", Text.showt fsOwnerId)
    ] []

instance HasPriority FileSaved where logData = logDebug . toLog

-- MessageSended -----------------------------------------------------------

newtype MessageSended = MessageSended Integer
  deriving (Generic, Aeson.FromJSON)

instance Loggable MessageSended where
  toLog (MessageSended id) = "Successfully sent message with id: "
    <> Text.showt id

instance HasPriority MessageSended where logData = logInfo . toLog
