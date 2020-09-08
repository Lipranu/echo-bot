{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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
import Data.Maybe          ( fromMaybe )
import Data.Text.Extended  ( Text )
import Data.Typeable       ( Typeable, typeOf )
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

instance Typeable a => Loggable (Response a) where
  toLog (Success x) =
    "Successfully received response of type: " <> Text.showt (typeOf x)

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
  toLog _ = "longpollserver placeholder"

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
  toLog (NewMessage m) = toLog m
  toLog (NotSupported t) = "Not supprted update of type: " <> t

-- Message -----------------------------------------------------------------

data Message = Message
  { mFromId      :: Integer
  , mPeerId      :: Integer
  , mText        :: Maybe Text
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
  toLog Message {..} = "New message recived:\n\
    \ | from_id: "     <> Text.showt mFromId      <> "\n\
    \ | peer_id: "     <> Text.showt mPeerId      <> "\n\
    \ | text: "        <> fromMaybe "" mText               --    <> "\n\
--    \ | attachments: " <> Text.showt mAttachments <> "\n\
--    \ | geo: "         <> Text.showt mGeo         <> "\n\
--    \ | keyboard: "    <> Text.showt mKeyboard

-- Attachment --------------------------------------------------------------

data Attachment
  = Attachment Text AttachmentBody
  | Document DocumentBody

instance Aeson.FromJSON Attachment where
  parseJSON = Aeson.withObject "App.Vk.Attachment" $ \o -> do
    aType      <- o .: "type"
    case aType of
      "doc" -> Document         <$> (o .: aType >>= Aeson.parseJSON)
      _     -> Attachment aType <$> (o .: aType >>= Aeson.parseJSON)

instance Loggable Attachment where
  toLog _ = "PlaceHolder"--"Proccessing attachment:\n\
--    \ | Type: "     <> aType               <> "\n\
--    \ | Media Id: " <> Text.showt aMediaId <> "\n\
--    \ | Owner Id: " <> Text.showt aOwnerId <> key
--    where key = case aAccessKey of
--            Just v  -> "\n | Access Key: " <> v
--            Nothing -> mempty

-- AttachmentBody ----------------------------------------------------------

data AttachmentBody = AttachmentBody
  { aId        :: Integer
  , aOwnerId   :: Integer
  , aAccessKey :: Maybe Text
  } deriving Generic

instance Aeson.FromJSON AttachmentBody where
  parseJSON = Aeson.parseJsonDrop

-- AttachmentBody ----------------------------------------------------------

data DocumentBody = DocumentBody
  { dUrl    :: Text
  , dTitle  :: Text
  } deriving Generic

instance Aeson.FromJSON DocumentBody where
  parseJSON = Aeson.parseJsonDrop

-- UploadServer ------------------------------------------------------------

newtype UploadServer = UploadServer Text

instance Aeson.FromJSON UploadServer where
  parseJSON = Aeson.withObject "App.Vk.UploadServer" $ \o ->
    UploadServer <$> o .: "upload_url"

-- FileUploaded ------------------------------------------------------------

data FileUploaded
  = FileUploaded Text
  | UploadError UploadErrorBody

instance Aeson.FromJSON FileUploaded where
  parseJSON = Aeson.withObject "App.Vk.FileUploaded" $ \o ->
        FileUploaded <$> o .: "file"
    <|> UploadError  <$> Aeson.parseJSON (Aeson.Object o)

instance Loggable FileUploaded where
  toLog _ = "Uploaded file placeholder"

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
  toLog _ = "uploaderrorbody placeholder"

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
  toLog _ = "FileSaved placeholder"
