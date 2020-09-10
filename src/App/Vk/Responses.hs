{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances   #-}

module App.Vk.Responses
  ( Attachment (..)
  , AttachmentBody (..)
  , DocumentBody (..)
  , FileSaved (..)
  , FileUploaded (..)
  , Message (..)
  , Response (..)
  , Update (..)
  , Updates (..)
  , UploadServer (..)
  , LongPollServer (..)
  ) where

-- IMPORTS -----------------------------------------------------------------

import Infrastructure.Logger ( Loggable (..) )

import Control.Applicative ( (<|>) )
import Data.Aeson          ( (.:), (.:?) )
import Data.Text.Extended  ( Text )
import GHC.Generics        ( Generic )

import qualified Data.Aeson.Extended as Aeson
import qualified Data.Text.Extended  as Text

-- TYPES AND INSTANCES -----------------------------------------------------

-- Response ----------------------------------------------------------------

data Response a
  = Success a
  | Error ErrorResponse

instance Aeson.FromJSON a => Aeson.FromJSON (Response a) where
  parseJSON = Aeson.withObject "App.Vk.Response" $ \o ->
        Success <$> o .: "response"
    <|> Error   <$> o .: "error"

instance Loggable a => Loggable (Response a) where
  toLog (Success x) = toLog x
  toLog (Error   x) = toLog x

-- ErrorResponse -----------------------------------------------------------

data ErrorResponse = ErrorResponse
  { eErrorCode     :: Integer
  , eErrorMsg      :: Text
  , eRequestParams :: [RequestParams]
  } deriving Generic

instance Aeson.FromJSON ErrorResponse where
  parseJSON = Aeson.parseJsonDrop

instance Loggable ErrorResponse where
  toLog ErrorResponse {..}
    = "An error occurred as a result of the request\n\
    \ | Error Code: "        <> Text.showt eErrorCode <> "\n\
    \ | Error Message: "     <> eErrorMsg             <> "\n\
    \ | Request Parameters:" <> params
    where params  = foldr (<>) "" $ fmap toLog eRequestParams

-- RequestParams -----------------------------------------------------------

data RequestParams = RequestParams
  { rpKey   :: Text
  , rpValue :: Text
  } deriving Generic

instance Aeson.FromJSON RequestParams where
  parseJSON = Aeson.parseJsonDrop

instance Loggable RequestParams where
  toLog RequestParams {..} = "\n | \t" <> rpKey <> ": " <> rpValue

-- LongPollServer ----------------------------------------------------------

data LongPollServer = LongPollServer
 { lpsKey    :: Text
 , lpsServer :: Text
 , lpsTs     :: Text
 } deriving Generic

instance Aeson.FromJSON LongPollServer where
  parseJSON = Aeson.parseJsonDrop

instance Loggable LongPollServer where
  toLog LongPollServer {..} = "Recived Long Poll Server:\n\
    \ | Server: "    <> lpsServer <> "\n\
    \ | Timestamp: " <> lpsTs     <> "\n\
    \ | Key: "       <> lpsKey

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
  toLog (Updates upds ts) = "Resived updates:\n\
    \ | Amount: "        <> (Text.showt . length) upds <> "\n\
    \ | New timestamp: " <> ts

  toLog (OutOfDate ts) =
    "Event history is outdated or partially lost. \
    \Performing new request for updates with timestamp: " <> Text.showt ts

  toLog KeyExpired = "Key expired. Performing request for new key"

  toLog DataLost   = "Information lost. Performing request for new key"

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
  toLog Message {..} = "Message data:\n\
    \ | From id: "     <> Text.showt mFromId              <> "\n\
    \ | Peer id: "     <> Text.showt mPeerId              <>
    maybeLoggable "Message" mMessage                      <>
    maybeLoggable "Latitude" (Text.showt <$> mLatitude)   <>
    maybeLoggable "Longitude" (Text.showt <$> mLongitude) <> "\n\
    \ | Attachments: " <> Text.showt (length mAttachments) -- <> "\n\
--    \ | Keyboard: "    <> Text.showt mKeyboard

-- Attachment --------------------------------------------------------------

data Attachment
  = Attachment AttachmentBody
  | Document DocumentBody
  | Sticker Integer

instance Aeson.FromJSON Attachment where
  parseJSON = Aeson.withObject "App.Vk.Attachment" $ \o -> do
    aType      <- o .: "type"
    case aType of
      "doc"     -> Document <$> (o .: aType >>= Aeson.parseJSON)
      "sticker" -> Sticker  <$> (o .: aType >>= (.: "sticker_id"))
      _     -> do
        body <- o .: aType >>= Aeson.parseJSON
        return $ Attachment $ body aType

instance Loggable Attachment where
  toLog (Attachment _) = "Processing attachment of type: Attachment"
  toLog (Document   _) = "Processing attachment of type: Document"
  toLog (Sticker    _) = "Processing attachment of type: Sticker"

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
  toLog AttachmentBody {..} = "Processing AttachmentBody:\n\
    \ | Type: "     <> aType               <> "\n\
    \ | Owner id: " <> Text.showt aOwnerId <> "\n\
    \ | Media id: " <> Text.showt aId      <>
    maybeLoggable "Access Key" aAccessKey

-- DocumentBody ------------------------------------------------------------

data DocumentBody = DocumentBody
  { dUrl    :: Text
  , dTitle  :: Text
  } deriving Generic

instance Aeson.FromJSON DocumentBody where
  parseJSON = Aeson.parseJsonDrop

instance Loggable DocumentBody where
  toLog DocumentBody {..} = "Processing DocumentBody:\n\
    \ | Title: " <> dTitle <> "\n\
    \ | Url: "   <> dUrl

-- UploadServer ------------------------------------------------------------

newtype UploadServer = UploadServer Text

instance Aeson.FromJSON UploadServer where
  parseJSON = Aeson.withObject "App.Vk.UploadServer" $ \o ->
    UploadServer <$> o .: "upload_url"

instance Loggable UploadServer where
  toLog (UploadServer url) = "Recived upload server:\n\
    \ | Url: " <> url

-- FileUploaded ------------------------------------------------------------

data FileUploaded
  = FileUploaded Text
  | UploadError UploadErrorBody

instance Aeson.FromJSON FileUploaded where
  parseJSON = Aeson.withObject "App.Vk.FileUploaded" $ \o ->
        FileUploaded <$> o .: "file"
    <|> UploadError  <$> Aeson.parseJSON (Aeson.Object o)

instance Loggable FileUploaded where
  toLog (FileUploaded text) = "File uploaded:\n\
    \ | Response body: " <> text
  toLog (UploadError body) = toLog body

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
  toLog UploadErrorBody {..} = "Error occurred during upload file:\n\
    \ | Error: "  <> uebError             <> "\n\
    \ | Bwact: "  <> uebBwact             <> "\n\
    \ | Server: " <> Text.showt uebServer <> "\n\
    \ | _sig: "   <> ueb_sig

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
  toLog FileSaved {..} = "File saved:\n\
    \ | Type: "     <> fsType               <> "\n\
    \ | Media id: " <> Text.showt fsMediaId <> "\n\
    \ | Owner id: " <> Text.showt fsOwnerId

-- FUNCTIONS ---------------------------------------------------------------

maybeLoggable :: Text -> Maybe Text -> Text
maybeLoggable _ Nothing = ""
maybeLoggable key (Just value)
  | Text.null value = ""
  | otherwise       = "\n | " <> key <> ": " <> value
