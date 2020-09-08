{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module App.Vk.Requests
  ( GetFile (..)
  , GetLongPollServer (..)
  , GetUpdates (..)
  , GetUploadServer (..)
  , SaveFile (..)
  , SendMessage (..)
  , UploadFile (..)
  ) where

-- IMPORTS -----------------------------------------------------------------

import App.Vk.Internal
import Infrastructure.Logger       (Loggable (..))
import Infrastructure.Requester    (ToRequest (..))
import Internal

import Control.Monad.IO.Class      ( MonadIO, liftIO )
import Control.Monad.Reader        ( MonadReader )
import Data.Text.Encoding.Extended ( encodeUtf8, encodeShowUtf8 )
import Data.Text.Extended          ( Text )

import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Lazy                  as LBS
import qualified Data.Text.Extended                    as Text
import qualified Network.HTTP.Client.Extended          as HTTP
import qualified Network.HTTP.Client.MultipartFormData as MP

-- TYPES AND INSTANCES -----------------------------------------------------

-- GetLongPollServer -------------------------------------------------------

data GetLongPollServer = GetLongPollServer

instance VkReader r m => ToRequest m r GetLongPollServer where
  toRequest GetLongPollServer = HTTP.urlEncodedBody
    <$> defaultBody
    <*> pure defaultRequest
          { HTTP.path = "/method/groups.getLongPollServer" }

instance Loggable GetLongPollServer where
  toLog _ = "Requesting long poll server"

-- GetUpdates --------------------------------------------------------------

data GetUpdates = GetUpdates
  { guKey  :: Text
  , guTs   :: Text
  , guPath :: Text
  , guHost :: Text
  }

instance MonadReader r m => ToRequest m r GetUpdates where
  toRequest GetUpdates {..} = return $ mkBody $ defaultRequest
    { HTTP.path = encodeUtf8 guPath
    , HTTP.host = encodeUtf8 guHost
    }
    where mkBody = HTTP.urlEncodedBody
                 [ ("act" , "a_check")
                 , ("key" , encodeUtf8 guKey)
                 , ("wait", "25")
                 , ("ts"  , encodeUtf8 guTs)
                 , ("mode", "2")
                 ]

instance Loggable GetUpdates where
  toLog _ = "Requesting updates from long poll server"

-- SendMessage -------------------------------------------------------------

data SendMessage = SendMessage
  { smPeerId      :: Integer
  , smRandomId    :: Int
  , smMessage     :: Maybe Text
  , smLatitude    :: Maybe Double
  , smLongitude   :: Maybe Double
  , smAttachments :: [Text]
  }

instance VkReader r m => ToRequest m r SendMessage where
  toRequest SendMessage {..} = do
    df <- defaultBody
    return $ HTTP.urlEncodedBody
      (mergeBodies mBody $ body <> df)
      defaultRequest
        { HTTP.method = "POST"
        , HTTP.path   = "method/messages.send"
        }
    where body       = [ ("peer_id"  , encodeShowUtf8 smPeerId)
                       , ("random_id", encodeShowUtf8 smRandomId)
                       ]
          mBody      = [ ("attachment", mAttach smAttachments)
                       , ("message"   , encodeUtf8     <$> smMessage)
                       , ("lat"       , encodeShowUtf8 <$> smLatitude)
                       , ("long"      , encodeShowUtf8 <$> smLongitude)
                       ]
          mAttach [] = Nothing
          mAttach xs = Just
            $ encodeUtf8
            $ Text.intercalate ","
            $ reverse xs

-- GetFile -----------------------------------------------------------------

newtype GetFile = GetFile Text

instance MonadReader r m => ToRequest m r GetFile where
  toRequest (GetFile url) = return $ HTTP.parseRequest_ $ Text.unpack url

-- GetUploadServer ---------------------------------------------------------

data GetUploadServer = GetUploadServer
  { gusType :: Text
  , gusPeerId :: Integer
  }

instance VkReader r m => ToRequest m r GetUploadServer where
  toRequest GetUploadServer {..} = do
    df <- defaultBody
    return $ HTTP.urlEncodedBody (body <> df)
           $ defaultRequest
      { HTTP.method = "GET"
      , HTTP.path   = "/method/docs.getMessagesUploadServer"
      }
    where body = [ ("type"   , encodeUtf8 gusType)
                 , ("peer_id", encodeShowUtf8 gusPeerId)
                 ]

-- UploadFile ----------------------------------------------------------

data UploadFile = UploadFile
  { udFile     :: LBS.ByteString
  , udUrl      :: Text
  , udFileName :: Text
  }

instance (MonadReader r m, MonadIO m) => ToRequest m r UploadFile where
  toRequest UploadFile {..} =
    let req   = HTTP.parseRequest_ $ Text.unpack udUrl
        part  = MP.partLBS "file" udFile
        partm = part { MP.partFilename = Just $ Text.unpack udFileName }
     in liftIO $ MP.formDataBody [partm] req

-- SaveFile ----------------------------------------------------------------

data SaveFile = SaveFile
  { sfFile  :: Text
  , sfTitle :: Text
  }

instance VkReader r m => ToRequest m r SaveFile where
  toRequest SaveFile {..} = do
    df <- defaultBody
    return $ HTTP.urlEncodedBody (body <> df) defaultRequest
      { HTTP.path = "/method/docs.save" }
    where body = [ ("file" , encodeUtf8 sfFile)
                 , ("title", encodeUtf8 sfTitle)
                 ]

-- FUNCTIONS ---------------------------------------------------------------

mergeBodies :: [(a, Maybe b)] -> [(a, b)] -> [(a, b)]
mergeBodies mBody body = foldr filterMaybe body mBody
  where filterMaybe (_, Nothing)       xs = xs
        filterMaybe (name, Just value) xs = (name, value) : xs

defaultBody :: VkReader r m => m [(BS.ByteString, BS.ByteString)]
defaultBody = do
  token <- obtain
  group <- obtain
  return
    [ ("access_token", encodeUtf8 $ unToken token)
    , ("group_id"    , encodeUtf8 $ unGroup group)
    , ("v"           , "5.122")
    ]

defaultRequest :: HTTP.Request
defaultRequest = HTTP.defaultRequest
  { HTTP.host   = "api.vk.com"
  , HTTP.method = "POST"
  }
