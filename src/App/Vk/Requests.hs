{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}

module App.Vk.Requests
  ( GetFile (..)
  , GetLongPollServer (..)
  , GetUpdates (..)
  , GetUploadServer (..)
  , SaveFile (..)
  , SendMessage (..)
  , GetName (..)
  , UploadFile (..)
  , Keyboard (..)
  , Button (..)
  , Action (..)
  ) where

-- IMPORTS -----------------------------------------------------------------

import App.Vk.Config    ( VkReader, Token (..), Group (..) )
import App.Vk.Responses ( FromId (..), PeerId (..), MessageId (..) )

import Infrastructure.Has
import Infrastructure.Logger
import Infrastructure.Requester

import Control.Monad.Catch    ( MonadThrow )
import Control.Monad.IO.Class ( MonadIO (..) )
import Data.Aeson.Extended    ( DropPrefix (..), ToJSON (..), encode )
import Data.ByteString.Lazy   ( toStrict )
import Data.Maybe             ( catMaybes )
import Data.Text.Encoding     ( encodeUtf8 )
import Data.Text.Extended     ( Text, showt, unpack )
import GHC.Generics           ( Generic )

import qualified Data.ByteString                       as BS
import qualified Network.HTTP.Client                   as HTTP
import qualified Network.HTTP.Client.MultipartFormData as MP

-- CLASSES -----------------------------------------------------------------

class ToRequestValue a where
  toValue :: a -> BS.ByteString

class ToRequestBody a where
  toBody :: a -> [(BS.ByteString, BS.ByteString)]

class ToRequestFields a where
  mkRequest :: a -> HTTP.Request

-- TYPES AND INSTANCES -----------------------------------------------------

instance ToRequestValue Text where
  toValue = encodeUtf8

instance ToRequestValue Integer where
  toValue = encodeUtf8 . showt

instance ToRequestValue Int where
  toValue = encodeUtf8 . showt

instance ToRequestValue Double where
  toValue = encodeUtf8 . showt

instance ToRequestValue PeerId where
  toValue = encodeUtf8 . showt . getPeerId

instance ToRequestValue FromId where
  toValue = encodeUtf8 . showt . getFromId

instance ToRequestValue MessageId where
  toValue = encodeUtf8 . showt . getMessageId

-- GetLongPollServer -------------------------------------------------------

data GetLongPollServer = GetLongPollServer

instance ToRequestBody GetLongPollServer where toBody _ = []

instance ToRequestFields GetLongPollServer where
  mkRequest _ = defaultRequest
    { HTTP.path   = "/method/groups.getLongPollServer"
    , HTTP.method = "GET"
    }

instance (Monad m, VkReader r m) => ToRequest m GetLongPollServer where
  toRequest = requestBuilder

instance Loggable GetLongPollServer where
  logData _ = logInfo "Requesting long poll server"

-- GetUpdates --------------------------------------------------------------

data GetUpdates = GetUpdates
  { guKey  :: Text
  , guTs   :: Text
  , guPath :: Text
  , guHost :: Text
  }

instance ToRequestBody GetUpdates where
  toBody GetUpdates {..} =
    [ ("key" , toValue guKey)
    , ("ts"  , toValue guTs)
    , ("act" , "a_check")
    , ("wait", "25")
    , ("mode", "2")
    ]

instance ToRequestFields GetUpdates where
  mkRequest GetUpdates {..} = defaultRequest
    { HTTP.path   = encodeUtf8 guPath
    , HTTP.host   = encodeUtf8 guHost
    , HTTP.method = "GET"
    }

instance Monad m => ToRequest m GetUpdates where
  toRequest gu = pure $ HTTP.urlEncodedBody (toBody gu) $ mkRequest gu

instance Loggable GetUpdates where
  logData GetUpdates {..} = logInfo $ "Requesting updates with ts: " <> guTs

-- SendMessage -------------------------------------------------------------

data SendMessage = SendMessage
  { smPeerId      :: PeerId
  , smRandomId    :: Int
  , smMessage     :: Maybe Text
  , smLatitude    :: Maybe Double
  , smLongitude   :: Maybe Double
  , smReplyId     :: Maybe Integer
  , smForwardsId  :: Maybe Text
  , smAttachments :: Maybe Text
  , smStickerId   :: Maybe Integer
  , smKeyboard    :: Maybe Keyboard
  }

instance ToRequestBody SendMessage where
  toBody SendMessage {..} = body <> catMaybes mBody
    where mValue name value = (name,) . toValue <$> value
          body  = [ ("peer_id"  , toValue smPeerId)
                  , ("random_id", toValue smRandomId)
                  ]
          mBody = [ mValue "message"          smMessage
                  , mValue "lat"              smLatitude
                  , mValue "long"             smLongitude
                  , mValue "sticker_id"       smStickerId
                  , mValue "attachment"       smAttachments
                  , mValue "forward_messages" smForwardsId
                  , mValue "reply_to"         smReplyId
                  , mValue "keyboard"         smKeyboard
                  ]

instance ToRequestFields SendMessage where
  mkRequest _ = defaultRequest { HTTP.path = "method/messages.send" }

instance (Monad m, VkReader r m) => ToRequest m SendMessage where
  toRequest = requestBuilder

instance Loggable SendMessage where
  logData SendMessage {..} = logInfo $ "Sending message with peer id: "
    <> showt (getPeerId smPeerId)

-- Keyboard ----------------------------------------------------------------

data Keyboard = Keyboard
  { kOneTime :: Bool
  , kInline  :: Bool
  , kButtons :: [[Button]]
  } deriving stock Generic
    deriving ToJSON via DropPrefix Keyboard

instance ToRequestValue Keyboard where
  toValue = toStrict . encode

-- Button ------------------------------------------------------------------

data Button = Button
  { bAction :: Action
  , bColor  :: Text
  } deriving stock Generic
    deriving ToJSON via DropPrefix Button

-- Action ------------------------------------------------------------------

data Action = Action
  { abType    :: Text
  , abLabel   :: Text
  , abPayload :: Text
  } deriving stock Generic
    deriving ToJSON via DropPrefix Action

-- GetName -----------------------------------------------------------------

newtype GetName = GetName FromId

instance ToRequestBody GetName where
  toBody (GetName id) = [("user_ids", toValue id)]

instance ToRequestFields GetName where
  mkRequest _ = defaultRequest
    { HTTP.method = "GET"
    , HTTP.path   = "/method/users.get"
    }

instance (Monad m, VkReader r m) => ToRequest m GetName where
  toRequest = requestBuilder

instance Loggable GetName where
  logData (GetName id) = logInfo $
    "Performing a request for a username with an id: "
    <> showt (getFromId id)

-- GetFile -----------------------------------------------------------------

newtype GetFile = GetFile Text

instance MonadThrow m => ToRequest m GetFile where
  toRequest (GetFile url) = HTTP.parseRequest $ unpack url

instance Loggable GetFile where
  logData _ = logInfo "Downloading file from attachment url"

-- GetUploadServer ---------------------------------------------------------

data GetUploadServer
  = FileUploadServer Text PeerId
  | PhotoUploadServer

instance ToRequestFields GetUploadServer where
  mkRequest x = defaultRequest
    { HTTP.method = "GET"
    , HTTP.path   = path <> ".getMessagesUploadServer"
    }
    where path = "/method/" <> case x of
            FileUploadServer _ _ -> "docs"
            PhotoUploadServer    -> "photos"

instance ToRequestBody GetUploadServer where
  toBody PhotoUploadServer = [("peer_id", "0")]
  toBody (FileUploadServer fType peerId) =
    [ ("peer_id", toValue peerId)
    , ("type"   , toValue fType)
    ]

instance (Monad m, VkReader r m) => ToRequest m GetUploadServer where
  toRequest = requestBuilder

instance Loggable GetUploadServer where
  logData PhotoUploadServer      = logInfo "Requesting photo upload server"
  logData (FileUploadServer t _) = logInfo $
    "Requesting docs upload server, file type: " <> t

-- UploadFile --------------------------------------------------------------

data UploadFile = UploadFile
  { ufFile  :: BS.ByteString
  , ufUrl   :: Text
  , ufTitle :: Text
  , ufType  :: Text
  }

instance (MonadThrow m, MonadIO m) => ToRequest m UploadFile where
  toRequest UploadFile {..} = do
    request <- HTTP.parseRequest $ unpack ufUrl
    liftIO $ MP.formDataBody
      [part { MP.partFilename = Just $ unpack ufTitle }]
      request
    where part = flip MP.partBS ufFile $ case ufType of
            "photo" -> "photo"
            _       -> "file"

instance Loggable UploadFile where
  logData UploadFile {..} = logInfo $ "Uploading file: " <> ufTitle

-- SaveFile ----------------------------------------------------------------

data SaveFile
  = SaveDocument Text Text
  | SavePhoto Text Integer Text Text

instance ToRequestFields SaveFile where
  mkRequest sf = defaultRequest { HTTP.path = "/method/" <> path }
    where path = case sf of
            SaveDocument { } -> "docs.save"
            SavePhoto    { } -> "photos.saveMessagesPhoto"

instance ToRequestBody SaveFile where
  toBody (SaveDocument title file) =
    [ ("file" , toValue file)
    , ("title", toValue title)
    ]
  toBody (SavePhoto _ server hash photo) =
    [ ("server", toValue server)
    , ("hash"  , toValue hash)
    , ("photo" , toValue photo)
    ]

instance (Monad m, VkReader r m) => ToRequest m SaveFile where
  toRequest = requestBuilder

instance Loggable SaveFile where
  logData (SaveDocument title _)  = logInfo $ "Saving document: " <> title
  logData (SavePhoto title _ _ _) = logInfo $ "Saving photo: "    <> title

-- FUNCTIONS ---------------------------------------------------------------

defaultRequest :: HTTP.Request
defaultRequest = HTTP.defaultRequest
  { HTTP.host   = "api.vk.com"
  , HTTP.method = "POST"
  , HTTP.secure = True
  , HTTP.port   = 443
  }

requestBuilder :: (VkReader r m, ToRequestFields a, ToRequestBody a)
               => a
               -> m HTTP.Request
requestBuilder x = do
  token <- ("access_token",) . toValue . unToken <$> obtain
  group <- ("group_id",)     . toValue . unGroup <$> obtain
  pure $ HTTP.urlEncodedBody (v : group : token : toBody x) $ mkRequest x
  where v = ("v", "5.124")
