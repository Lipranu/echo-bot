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
  , DocumentBody (..)
  , FileSaved (..)
  , FileUploaded (..)
  , LongPollServer (..)
  , Message (..)
  , MessageSended (..)
  , RawFile (..)
  , ResponseException
  , Update (..)
  , Updates (..)
  , UploadException
  , UploadServer (..)
  , UserName (..)
  , WallBody (..)
  , Payload (..)
  , Command (..)
  ) where

-- IMPORTS -----------------------------------------------------------------

import App.Shared.Responses

import Internal
import Infrastructure.Logger

import Control.Applicative  ( (<|>) )
import Control.Monad.Catch  ( Exception )
import Data.Aeson           ( (.:), (.:?) )
import Data.ByteString.Lazy ( ByteString )
import Data.Text.Extended   ( Text )
import GHC.Generics         ( Generic )

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
  , mPayload     :: Maybe Payload
  }

instance Aeson.FromJSON Message where
  parseJSON = Aeson.withObject "App.Vk.Message" $ \o -> Message
    <$> o .:  "from_id"
    <*> o .:  "peer_id"
    <*> o .:? "text"
    <*> (coord o "latitude"  <|> pure Nothing)
    <*> (coord o "longitude" <|> pure Nothing)
    <*> o .:  "attachments"
    <*> o .:? "payload"
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
  parseJSON = Aeson.withText "App.Vk.Responses.Payload" $ \t -> Payload t
    <$> Aeson.parseJSON (Aeson.String t)

instance Loggable Payload where toLog (Payload t _) = t

-- Command -----------------------------------------------------------------

data Command
  = Help
  | Repeat
  | NewCount Int
  | UnknownCommand Text

instance Aeson.FromJSON Command where
  parseJSON = Aeson.withText "App.Vk.Responses.Payload.Command" $ \case
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
  , wToId      :: Integer
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
