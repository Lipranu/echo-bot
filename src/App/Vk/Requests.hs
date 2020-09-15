{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric            #-}

module App.Vk.Requests
  ( GetFile (..)
  , GetLongPollServer (..)
  , GetUpdates (..)
  , GetUploadServer (..)
  , SaveFile (..)
  , SendMessage (..)
  --, SendMessage' (..)
  , UploadFile (..)
  , Keyboard (..)
  , Button (..)
  , Action (..)
  ) where

-- IMPORTS -----------------------------------------------------------------

import App.Vk.Internal
import Infrastructure.Logger       ( Loggable (..), HasPriority (..)
                                   , logInfo )
import Infrastructure.Requester    ( ToRequest (..) )
import Internal

import Control.Monad.IO.Class      ( MonadIO, liftIO )
import Control.Monad.Reader        ( MonadReader )
import Data.Text.Encoding.Extended ( encodeUtf8, encodeShowUtf8 )
import Data.Text.Extended          ( Text )
import GHC.Generics (Generic)
--import Network.HTTP.Types.Header (hContentEncoding)

import qualified Data.Aeson.Extended                   as Aeson
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Lazy                  as LBS
import qualified Data.Text.Extended                    as Text
import qualified Network.HTTP.Client                   as HTTP
import qualified Network.HTTP.Client.MultipartFormData as MP

-- TYPES AND INSTANCES -----------------------------------------------------

-- GetLongPollServer -------------------------------------------------------

data GetLongPollServer = GetLongPollServer

instance VkReader r m => ToRequest m r GetLongPollServer where
  toRequest GetLongPollServer = HTTP.urlEncodedBody
    <$> defaultBody
    <*> pure request
    where request = defaultRequest
                    { HTTP.path   = "/method/groups.getLongPollServer"
                    , HTTP.method = "GET"
                    }

instance Loggable GetLongPollServer where
  toLog _ = "Requesting long poll server"

instance HasPriority GetLongPollServer where logData = logInfo . toLog

-- GetUpdates --------------------------------------------------------------

data GetUpdates = GetUpdates
  { guKey  :: Text
  , guTs   :: Text
  , guPath :: Text
  , guHost :: Text
  }

instance MonadReader r m => ToRequest m r GetUpdates where
  toRequest GetUpdates {..} = return $ body request
    where body    = HTTP.urlEncodedBody
                    [ ("act" , "a_check")
                    , ("key" , encodeUtf8 guKey)
                    , ("wait", "25")
                    , ("ts"  , encodeUtf8 guTs)
                    , ("mode", "2")
                    ]
          request = defaultRequest
                    { HTTP.path   = encodeUtf8 guPath
                    , HTTP.host   = encodeUtf8 guHost
                    , HTTP.method = "GET"
                    }

instance Loggable GetUpdates where
  toLog GetUpdates {..} = "Requesting updates with ts: " <> guTs

instance HasPriority GetUpdates where logData = logInfo . toLog

-- SendMessage -------------------------------------------------------------

data SendMessage = SendMessage
  { smPeerId      :: Integer
  , smRandomId    :: Int
  , smMessage     :: Maybe Text
  , smLatitude    :: Maybe Double
  , smLongitude   :: Maybe Double
  , smAttachments :: Maybe Text
  , smSticker     :: Maybe Integer
  , smKeyboard    :: Maybe Keyboard
  }

instance VkReader r m => ToRequest m r SendMessage where
  toRequest SendMessage {..} = do
    df <- defaultBody
    return $ HTTP.urlEncodedBody (mergeBodies mBody $ body <> df) request
    where body     = [ ("peer_id"   , encodeShowUtf8 smPeerId)
                     , ("random_id" , encodeShowUtf8 smRandomId)
                     ]
          mBody    = [ ("attachment", encodeUtf8     <$> smAttachments)
                     , ("message"   , encodeUtf8     <$> smMessage)
                     , ("lat"       , encodeShowUtf8 <$> smLatitude)
                     , ("long"      , encodeShowUtf8 <$> smLongitude)
                     , ("sticker_id", encodeShowUtf8 <$> smSticker)
                     , ("keyboard"  , keyboard)
                     ]
          keyboard = (LBS.toStrict . Aeson.encode) <$> smKeyboard
          request  = defaultRequest
                     { HTTP.method = "POST"
                     , HTTP.path   = "method/messages.send"
                     }

instance Loggable SendMessage where
  toLog SendMessage {..} = "Sending message with peer id: "
    <> Text.showt smPeerId

instance HasPriority SendMessage where logData = logInfo . toLog

-- Keyboard ----------------------------------------------------------------

data Keyboard = Keyboard
  { kOneTime :: Bool
  , kInline  :: Bool
  , kButtons :: [[Button]]
  } deriving Generic

instance Aeson.ToJSON Keyboard where
  toJSON = Aeson.toJsonDrop

-- Button ------------------------------------------------------------------

data Button = Button
  { bAction :: Action
  , bColor  :: Text
  } deriving Generic

instance Aeson.ToJSON Button where
  toJSON = Aeson.toJsonDrop

-- Button ------------------------------------------------------------------

data Action = Action
  { abType    :: Text
  , abLabel   :: Text
  , abPayload :: Text
  } deriving Generic

instance Aeson.ToJSON Action where
  toJSON = Aeson.toJsonDrop

-- GetName -----------------------------------------------------------------

newtype GetName = GetName Integer

instance VkReader r m => ToRequest m r GetName where
  toRequest (GetName id) = do
    df <- defaultBody
    return $ HTTP.urlEncodedBody (body <> df) request
    where body    = [ ("user_ids"   , encodeShowUtf8 id) ]
          request = defaultRequest
                    { HTTP.method = "GET"
                    , HTTP.path   = "/method/users.get"
                    }

instance Loggable GetName where
  toLog (GetName id) = "Performing a request for a username with an id: "
    <> Text.showt id

instance HasPriority GetName where logData = logInfo . toLog

-- GetFile -----------------------------------------------------------------

newtype GetFile = GetFile Text

instance MonadReader r m => ToRequest m r GetFile where
  toRequest (GetFile url) = return $ HTTP.parseRequest_ $ Text.unpack url

instance Loggable GetFile where
  toLog _ = "Downloading file from attachment url"

instance HasPriority GetFile where logData = logInfo . toLog

-- GetUploadServer ---------------------------------------------------------

data GetUploadServer = GetUploadServer
  { gusType   :: Text
  , gusPeerId :: Integer
  }

instance VkReader r m => ToRequest m r GetUploadServer where
  toRequest GetUploadServer {..} = do
    df <- defaultBody
    return $ HTTP.urlEncodedBody (body <> df) request
    where body    = [ ("type"   , encodeUtf8     gusType)
                    , ("peer_id", encodeShowUtf8 gusPeerId)
                    ]
          request = defaultRequest
                    { HTTP.method = "GET"
                    , HTTP.path   = "/method/docs.getMessagesUploadServer"
                    }

instance Loggable GetUploadServer where
  toLog GetUploadServer {..} = "Requesting upload server of type: "
    <> gusType

instance HasPriority GetUploadServer where logData = logInfo . toLog

-- UploadFile ----------------------------------------------------------

data UploadFile = UploadFile
  { ufFile  :: BS.ByteString
  , ufUrl   :: Text
  , ufTitle :: Text
  }

instance (MonadReader r m, MonadIO m) => ToRequest m r UploadFile where
  toRequest UploadFile {..} =
    let request = HTTP.parseRequest_ $ Text.unpack ufUrl
        part    = MP.partBS "file" ufFile
     in liftIO $ MP.formDataBody
        [part { MP.partFilename = Just $ Text.unpack ufTitle }]
        request

instance Loggable UploadFile where
  toLog UploadFile {..} = "Uploading file: " <> ufTitle

instance HasPriority UploadFile where logData = logInfo . toLog

-- SaveFile ----------------------------------------------------------------

data SaveFile = SaveFile
  { sfFile  :: Text
  , sfTitle :: Text
  }

instance VkReader r m => ToRequest m r SaveFile where
  toRequest SaveFile {..} = do
    df <- defaultBody
    return $ HTTP.urlEncodedBody (body <> df) request
    where body    = [ ("file" , encodeUtf8 sfFile)
                    , ("title", encodeUtf8 sfTitle)
                    ]
          request = defaultRequest
                    { HTTP.path   = "/method/docs.save"
                    , HTTP.method = "POST"
                    }

instance Loggable SaveFile where
  toLog SaveFile {..} = "Saving file: " <> sfTitle

instance HasPriority SaveFile where logData = logInfo . toLog

-- FUNCTIONS ---------------------------------------------------------------

mergeBodies :: [(a, Maybe b)] -> [(a, b)] -> [(a, b)]
mergeBodies mBody body = foldr filterMaybe body mBody
  where filterMaybe (_, Nothing)       xs = xs
        filterMaybe (name, Just value) xs = (name, value) : xs

defaultBody :: VkReader r m => m [(BS.ByteString, BS.ByteString)]
defaultBody = do
  token <- obtain
  group <- obtain
  return [ ("access_token", encodeUtf8 $ unToken token)
         , ("group_id"    , encodeUtf8 $ unGroup group)
         , ("v"           , "5.122")
         ]

defaultRequest :: HTTP.Request
defaultRequest = HTTP.defaultRequest
  { HTTP.host   = "api.vk.com"
  , HTTP.method = "POST"
  , HTTP.secure = True
  , HTTP.port   = 443
  }
