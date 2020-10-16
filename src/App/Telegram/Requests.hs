{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE UndecidableInstances  #-}

module App.Telegram.Requests
  ( GetUpdates (..)
  , SendAnimation (..)
  , SendAudio (..)
  , SendDocument (..)
  , SendMessage (..)
  , SendPhoto (..)
  , SendSticker (..)
  , SendVideo (..)
  , SendVideoNote (..)
  , SendVoice (..)
  ) where

-- IMPORTS -----------------------------------------------------------------

import App.Telegram.Config      ( TelegramReader, Token (..) )

import Infrastructure.Has
import Infrastructure.Logger
import Infrastructure.Requester

import Data.Aeson.Extended         ( ToJSON (..), toJsonDrop, encode )
import Data.Text.Encoding.Extended ( encodeUtf8, encodeShowUtf8 )
import Data.Text.Extended          ( Text, showt )
import GHC.Generics                ( Generic )
import Network.HTTP.Client         ( Request )

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Extended   as Text
import qualified Network.HTTP.Client  as HTTP

-- TYPES AND INSTANCES -----------------------------------------------------

-- GetUpdates --------------------------------------------------------------

newtype GetUpdates = GetUpdates (Maybe Integer)

instance (TelegramReader r m, Monad m) => ToRequest m GetUpdates where
  toRequest (GetUpdates Nothing) = do
    token <- obtain
    return $
      HTTP.urlEncodedBody defaultGetUpdatesBody $
      defaultRequest
      { HTTP.path = defaultPath token <> "/getUpdates"
      }

  toRequest (GetUpdates (Just n)) = do
    token <- obtain
    return $
      HTTP.urlEncodedBody mkBody $
      defaultRequest
      { HTTP.path = defaultPath token <> "/getUpdates" }
    where mkBody = ("offset" , encodeShowUtf8 $ n + 1)
                 : defaultGetUpdatesBody

instance Loggable GetUpdates where
  toLog (GetUpdates Nothing)
    = "GetUpdates request without offset"
  toLog (GetUpdates (Just n))
    = "GetUpdates request with offset: " <> Text.showt (n + 1)

instance HasPriority GetUpdates where logData = logInfo . toLog

-- SendMessage -------------------------------------------------------------

data SendMessage = SendMessage
  { smText             :: Text
  , smChatId           :: Integer
  , smParseMode        :: Text
  , smReplyToMessageId :: Bool
  } deriving Generic

instance ToJSON SendMessage where toJSON = toJsonDrop

instance (TelegramReader env m, Monad m) => ToRequest m SendMessage where
  toRequest = mkRequest "/sendMessage"

instance Loggable SendMessage where
  toLog SendMessage {..} = mkToLog "SendMessage:"
    [ ("Text"      , smText)
    , ("Chat Id"   , showt smChatId)
    , ("Parse Mode", smParseMode)
    , ("Reply Id"  , showt smReplyToMessageId)
    ] []

instance HasPriority SendMessage where
  logData m = logInfo ("Sending message" :: Text) >> logDebug (toLog m)

-- SendPhoto ---------------------------------------------------------------

data SendPhoto = SendPhoto
  { spPhoto            :: Text
  , spCaption          :: Maybe Text
  , spChatId           :: Integer
  , spParseMode        :: Text
  , spReplyToMessageId :: Bool
  } deriving Generic

instance ToJSON SendPhoto where toJSON = toJsonDrop

instance (TelegramReader env m, Monad m) => ToRequest m SendPhoto where
  toRequest = mkRequest "/sendPhoto"

instance Loggable SendPhoto where
  toLog SendPhoto {..} = mkToLog "SendPhoto:"
    [ ("Photo Id"  , showt spPhoto)
    , ("Chat Id"   , showt spChatId)
    , ("Parse Mode", spParseMode)
    , ("Reply Id"  , showt spReplyToMessageId)
    ] [("Caption", spCaption)]

instance HasPriority SendPhoto where
  logData m = logInfo ("Sending message with photo" :: Text)
    >> logDebug (toLog m)

-- SendVideo ---------------------------------------------------------------

data SendVideo = SendVideo
  { svVideo            :: Text
  , svCaption          :: Maybe Text
  , svChatId           :: Integer
  , svParseMode        :: Text
  , svReplyToMessageId :: Bool
  } deriving Generic

instance ToJSON SendVideo where toJSON = toJsonDrop

instance (TelegramReader env m, Monad m) => ToRequest m SendVideo where
  toRequest = mkRequest "/sendVideo"

instance Loggable SendVideo where
  toLog SendVideo {..} = mkToLog "SendVideo:"
    [ ("Video Id"  , showt svVideo)
    , ("Chat Id"   , showt svChatId)
    , ("Parse Mode", svParseMode)
    , ("Reply Id"  , showt svReplyToMessageId)
    ] [("Caption", svCaption)]

instance HasPriority SendVideo where
  logData m = logInfo ("Sending message with video" :: Text)
    >> logDebug (toLog m)

-- SendAudio ---------------------------------------------------------------

data SendAudio = SendAudio
  { saAudio            :: Text
  , saCaption          :: Maybe Text
  , saChatId           :: Integer
  , saParseMode        :: Text
  , saReplyToMessageId :: Bool
  } deriving Generic

instance ToJSON SendAudio where toJSON = toJsonDrop

instance (TelegramReader env m, Monad m) => ToRequest m SendAudio where
  toRequest = mkRequest "/sendAudio"

instance Loggable SendAudio where
  toLog SendAudio {..} = mkToLog "SendAudio:"
    [ ("Audio Id"  , showt saAudio)
    , ("Chat Id"   , showt saChatId)
    , ("Parse Mode", saParseMode)
    , ("Reply Id"  , showt saReplyToMessageId)
    ] [("Caption", saCaption)]

instance HasPriority SendAudio where
  logData m = logInfo ("Sending message with audio" :: Text)
    >> logDebug (toLog m)

-- SendAnimation -----------------------------------------------------------

data SendAnimation = SendAnimation
  { sanAnimation        :: Text
  , sanCaption          :: Maybe Text
  , sanChatId           :: Integer
  , sanParseMode        :: Text
  , sanReplyToMessageId :: Bool
  } deriving Generic

instance ToJSON SendAnimation where toJSON = toJsonDrop

instance (TelegramReader env m, Monad m) => ToRequest m SendAnimation where
  toRequest = mkRequest "/sendAnimation"

instance Loggable SendAnimation where
  toLog SendAnimation {..} = mkToLog "SendAnimation:"
    [ ("Animation Id", showt sanAnimation)
    , ("Chat Id"     , showt sanChatId)
    , ("Parse Mode"  , sanParseMode)
    , ("Reply Id"    , showt sanReplyToMessageId)
    ] [("Caption", sanCaption)]

instance HasPriority SendAnimation where
  logData m = logInfo ("Sending message with animation" :: Text)
    >> logDebug (toLog m)

-- SendAnimation -----------------------------------------------------------

data SendDocument = SendDocument
  { sdDocument         :: Text
  , sdCaption          :: Maybe Text
  , sdChatId           :: Integer
  , sdParseMode        :: Text
  , sdReplyToMessageId :: Bool
  } deriving Generic

instance ToJSON SendDocument where toJSON = toJsonDrop

instance (TelegramReader env m, Monad m) => ToRequest m SendDocument where
  toRequest = mkRequest "/sendDocument"

instance Loggable SendDocument where
  toLog SendDocument {..} = mkToLog "SendDocument:"
    [ ("Document Id", showt sdDocument)
    , ("Chat Id"    , showt sdChatId)
    , ("Parse Mode" , sdParseMode)
    , ("Reply Id"   , showt sdReplyToMessageId)
    ] [("Caption", sdCaption)]

instance HasPriority SendDocument where
  logData m = logInfo ("Sending message with document" :: Text)
    >> logDebug (toLog m)

-- SendVoice ---------------------------------------------------------------

data SendVoice = SendVoice
  { svcAudio            :: Text
  , svcCaption          :: Maybe Text
  , svcChatId           :: Integer
  , svcParseMode        :: Text
  , svcReplyToMessageId :: Bool
  } deriving Generic

instance ToJSON SendVoice where toJSON = toJsonDrop

instance (TelegramReader env m, Monad m) => ToRequest m SendVoice where
  toRequest = mkRequest "/sendVoice"

instance Loggable SendVoice where
  toLog SendVoice {..} = mkToLog "SendVoice:"
    [ ("Voice Id"  , showt svcAudio)
    , ("Chat Id"   , showt svcChatId)
    , ("Parse Mode", svcParseMode)
    , ("Reply Id"  , showt svcReplyToMessageId)
    ] [("Caption", svcCaption)]

instance HasPriority SendVoice where
  logData m = logInfo ("Sending message with voice" :: Text)
    >> logDebug (toLog m)

-- SendVideoNote -----------------------------------------------------------

data SendVideoNote = SendVideoNote
  { svnVideo            :: Text
  , svnCaption          :: Maybe Text
  , svnChatId           :: Integer
  , svnParseMode        :: Text
  , svnReplyToMessageId :: Bool
  } deriving Generic

instance ToJSON SendVideoNote where toJSON = toJsonDrop

instance (TelegramReader env m, Monad m) => ToRequest m SendVideoNote where
  toRequest = mkRequest "/sendVideoNote"

instance Loggable SendVideoNote where
  toLog SendVideoNote {..} = mkToLog "SendVideoNote:"
    [ ("Video Note Id", showt svnVideo)
    , ("Chat Id"      , showt svnChatId)
    , ("Parse Mode"   , svnParseMode)
    , ("Reply Id"     , showt svnReplyToMessageId)
    ] [("Caption", svnCaption)]

instance HasPriority SendVideoNote where
  logData m = logInfo ("Sending message with video note" :: Text)
    >> logDebug (toLog m)

-- SendSticker -------------------------------------------------------------

data SendSticker = SendSticker
  { ssSticker          :: Text
  , ssChatId           :: Integer
  , ssParseMode        :: Text
  , ssReplyToMessageId :: Bool
  } deriving Generic

instance ToJSON   SendSticker where toJSON = toJsonDrop

instance (TelegramReader env m, Monad m) => ToRequest m SendSticker where
  toRequest = mkRequest "/sendSticker"

instance Loggable SendSticker where
  toLog SendSticker {..} = mkToLog "SendSticker:"
    [ ("Sticker Id", showt ssSticker)
    , ("Chat Id"   , showt ssChatId)
    , ("Parse Mode", ssParseMode)
    , ("Reply Id"   , showt ssReplyToMessageId)
    ] []

instance HasPriority SendSticker where
  logData m = logInfo ("Sending message with sticker" :: Text)
    >> logDebug (toLog m)

-- FUNCTIONS ---------------------------------------------------------------

mkRequest :: (TelegramReader env m, ToJSON a) => Text -> a -> m Request
mkRequest path x = do
  token <- unToken <$> obtain
  pure defaultRequest
    { HTTP.path = "/bot" <> encodeUtf8 token <> encodeUtf8 path
    , HTTP.requestBody = HTTP.RequestBodyBS $ LBS.toStrict $ encode x
    , HTTP.requestHeaders = [("content-type","application/json")]
    }

defaultRequest :: HTTP.Request
defaultRequest = HTTP.defaultRequest
  { HTTP.host = "api.telegram.org"
  , HTTP.method = "POST"
  , HTTP.secure = True
  , HTTP.port   = 443
  }

defaultPath :: Token -> BS.ByteString
defaultPath token = "/bot" <> encodeUtf8 (unToken token)

defaultGetUpdatesBody :: [(BS.ByteString, BS.ByteString)]
defaultGetUpdatesBody =
  let list :: [Text]
      list = ["message", "channel_post", "callback_query"]
   in [ ("timeout", "25")
      , ("allowed_updates", LBS.toStrict $ encode list)
      ]
