{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE UndecidableInstances  #-}

module App.Vk.Requests
  ( GetFile (..)
  , GetLongPollServer (..)
  , GetUpdates (..)
  , GetUploadServer (..)
  , SaveFile (..)
  , SendMessage (..)
  , GetName (..)
  , PeerId (..)
  , UploadFile (..)
  , Keyboard (..)
  , Button (..)
  , Action (..)
  ) where

-- IMPORTS -----------------------------------------------------------------

import App.Vk.Config            ( VkReader, Token (..), Group (..) )

import Infrastructure.Has
import Infrastructure.Logger    ( Loggable (..), HasPriority (..), logInfo )
import Infrastructure.Requester ( ToRequest (..) )

import Control.Monad.IO.Class      ( MonadIO, liftIO )
import Control.Monad.State         ( MonadState )
import Control.Monad.Catch         ( MonadThrow )
import Data.Text.Encoding.Extended ( encodeUtf8, encodeShowUtf8 )
import Data.Text.Extended          ( Text )
import GHC.Generics                ( Generic )

import qualified Data.Aeson.Extended                   as Aeson
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Lazy                  as LBS
import qualified Data.Text.Extended                    as Text
import qualified Network.HTTP.Client                   as HTTP
import qualified Network.HTTP.Client.MultipartFormData as MP

-- TYPES AND INSTANCES -----------------------------------------------------

-- GetLongPollServer -------------------------------------------------------

data GetLongPollServer = GetLongPollServer

instance (Monad m, VkReader r m) => ToRequest m GetLongPollServer where
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

instance Monad m => ToRequest m GetUpdates where
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
  , smReplyId     :: Maybe Integer
  , smForwardsId  :: Maybe Text--[Integer]
  , smAttachments :: Maybe Text
  , smSticker     :: Maybe Integer
  , smKeyboard    :: Maybe Keyboard
  }

instance (Monad m, VkReader r m) => ToRequest m SendMessage where
  toRequest SendMessage {..} = do
    df <- defaultBody
    return $ HTTP.urlEncodedBody (mergeBodies mBody $ body <> df) request
    where body     = [ ("peer_id"         , encodeShowUtf8 smPeerId)
                     , ("random_id"       , encodeShowUtf8 smRandomId)
                   --  , ("forward_messages", forwards)
                     ]
          mBody    = [ ("attachment"      , encodeUtf8     <$> smAttachments)
                     , ("message"         , encodeUtf8     <$> smMessage)
                     , ("lat"             , encodeShowUtf8 <$> smLatitude)
                     , ("long"            , encodeShowUtf8 <$> smLongitude)
                     , ("sticker_id"      , encodeShowUtf8 <$> smSticker)
                     , ("reply_to"        , encodeShowUtf8 <$> smReplyId)
                     , ("forward_messages", encodeUtf8     <$> smForwardsId)
                     , ("keyboard"        , keyboard)
                     ]
          keyboard = LBS.toStrict . Aeson.encode <$> smKeyboard
          --forwards = LBS.toStrict $ Aeson.encode $ smForwardsId
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

-- Action ------------------------------------------------------------------

data Action = Action
  { abType    :: Text
  , abLabel   :: Text
  , abPayload :: Text
  } deriving Generic

instance Aeson.ToJSON Action where
  toJSON = Aeson.toJsonDrop

-- GetName -----------------------------------------------------------------

newtype GetName = GetName Integer

instance (Monad m, VkReader r m) => ToRequest m GetName where
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

instance MonadThrow m => ToRequest m GetFile where
  toRequest (GetFile url) = HTTP.parseRequest $ Text.unpack url

instance Loggable GetFile where
  toLog _ = "Downloading file from attachment url"

instance HasPriority GetFile where logData = logInfo . toLog

-- GetUploadServer ---------------------------------------------------------

newtype PeerId = PeerId Integer

data GetUploadServer
  = FileUploadServer Text
  | PhotoUploadServer

instance (Monad m, VkReader r m, MonadState s m, Has PeerId s)
  => ToRequest m GetUploadServer where
  toRequest gus = do
    df           <- defaultBody
    (PeerId pid) <- grab
    return $ HTTP.urlEncodedBody (body pid <> df) request
    where body pid = [("peer_id", "0")] <> case gus of
            FileUploadServer t -> [ ("type", encodeUtf8 t)
                                  , ("peer_id", encodeShowUtf8 pid)
                                  ]
            PhotoUploadServer  -> [ ("peer_id", "0") ]
          request  = defaultRequest
                     { HTTP.method = "GET"
                     , HTTP.path   = path <> ".getMessagesUploadServer"
                     }
          path     = "/method/" <> case gus of
            FileUploadServer _ -> "docs"
            PhotoUploadServer  -> "photos"

instance Loggable GetUploadServer where
  toLog PhotoUploadServer    = "Requesting photo upload server"
  toLog (FileUploadServer t) = "Requesting docs upload server, file type: "
    <> t

instance HasPriority GetUploadServer where logData = logInfo . toLog

-- UploadFile ----------------------------------------------------------

data UploadFile = UploadFile
  { ufFile  :: BS.ByteString
  , ufUrl   :: Text
  , ufTitle :: Text
  , ufType  :: Text
  }

instance (MonadThrow m, MonadIO m) => ToRequest m UploadFile where
  toRequest UploadFile {..} = do
    request <- HTTP.parseRequest $ Text.unpack ufUrl
    liftIO $ MP.formDataBody
      [part { MP.partFilename = Just $ Text.unpack ufTitle }]
      request
    where part = flip MP.partBS ufFile $ case ufType of
            "photo" -> "photo"
            rest    -> "file"

instance Loggable UploadFile where
  toLog UploadFile {..} = "Uploading file: " <> ufTitle

instance HasPriority UploadFile where logData = logInfo . toLog

-- SaveFile ----------------------------------------------------------------

data SaveFile
  = SaveDocument Text Text
  | SavePhoto Text Integer Text Text

instance (Monad m, VkReader r m) => ToRequest m SaveFile where
  toRequest sf = do
    df <- defaultBody
    return $ HTTP.urlEncodedBody (body <> df) request
    where body    = case sf of
            SaveDocument title file ->
              [ ("file" , encodeUtf8 file)
              , ("title", encodeUtf8 title)
              ]
            SavePhoto _ server hash photo ->
              [ ("server", encodeShowUtf8 server)
              , ("hash"  , encodeUtf8 hash)
              , ("photo" , encodeUtf8 photo)
              ]
          request = defaultRequest
                    { HTTP.path   = "/method/" <> path
                    , HTTP.method = "POST"
                    }
          path    = case sf of
            SaveDocument { } -> "docs.save"
            SavePhoto    { } -> "photos.saveMessagesPhoto"

instance Loggable SaveFile where
  toLog (SaveDocument title _)  = "Saving document: " <> title
  toLog (SavePhoto title _ _ _) = "Saving photo: "    <> title

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
         , ("v"           , "5.124")
         ]

defaultRequest :: HTTP.Request
defaultRequest = HTTP.defaultRequest
  { HTTP.host   = "api.vk.com"
  , HTTP.method = "POST"
  , HTTP.secure = True
  , HTTP.port   = 443
  }
