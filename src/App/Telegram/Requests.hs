{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE UndecidableInstances  #-}

module App.Telegram.Requests
  ( GetUpdates (..)
  , SendCommonPart (..)
  , SendLocationBody (..)
  , SendMessageBody (..)
  , SendContactBody (..)
  , SendRequest (..)
  , SendVenueBody (..)
  , SendDiceBody (..)
  ) where

-- IMPORTS -----------------------------------------------------------------

import App.Telegram.Config      ( TelegramReader, Token (..) )
import App.Telegram.Responses   ( FileId (..) )

import Infrastructure.Has
import Infrastructure.Logger
import Infrastructure.Requester

import Data.Aeson.Extended         ( ToJSON (..), (.=), toJsonDrop, encode )
import Data.Text.Encoding.Extended ( encodeUtf8, encodeShowUtf8 )
import Data.Text.Extended          ( Text, showt )
import GHC.Generics                ( Generic )
import Network.HTTP.Client         ( Request )

import qualified Data.Aeson.Extended  as Aeson
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

-- SendRequest -------------------------------------------------------------

type ChatId  = Integer

data SendRequest
  = SendMessage   SendMessageBody
  | SendLocation  SendLocationBody
  | SendVenue     SendVenueBody
  | SendDice      SendDiceBody
  | SendContact   SendContactBody
  | SendSticker   FileId ChatId
  | SendAnimation FileId SendCommonPart
  | SendAudio     FileId SendCommonPart
  | SendDocument  FileId SendCommonPart
  | SendPhoto     FileId SendCommonPart
  | SendVoice     FileId SendCommonPart
  | SendVideo     FileId SendCommonPart
  | SendVideoNote FileId SendCommonPart
--TODO: | MediaGroup
--TODO: | Poll

instance ToJSON SendRequest where
  toJSON sr = case sr of
    SendMessage   body      -> toJSON body
    SendVenue     body      -> toJSON body
    SendLocation  body      -> toJSON body
    SendContact   body      -> toJSON body
    SendDice      body      -> toJSON body
    SendSticker   id chat   -> encodeSticker id chat
    SendAnimation id common -> commonEncode  id common "animation"
    SendAudio     id common -> commonEncode  id common "audio"
    SendDocument  id common -> commonEncode  id common "document"
    SendPhoto     id common -> commonEncode  id common "photo"
    SendVoice     id common -> commonEncode  id common "voice"
    SendVideo     id common -> commonEncode  id common "video"
    SendVideoNote id common -> commonEncode  id common "video_note"
--TODO: | MediaGroup
--TODO: | Poll
    where
      addChat chat vs = "chat_id" .= chat : vs

      encodeSticker (FileId id) chat = Aeson.object
        $ addChat chat ["sticker" .= id]

      commonEncode (FileId id) common srtype = Aeson.object
        $ srtype .= id : commonPart common

      commonPart SendCommonPart {..} = addChat chatId $
        "parse_mode" .= parseMode : case caption of
          Nothing -> []
          Just x  -> ["caption" .= x]

instance (TelegramReader env m, Monad m) => ToRequest m SendRequest where
  toRequest sr = mkRequest sr $ case sr of
    SendMessage   {} -> "/sendMessage"
    SendAnimation {} -> "/sendAnimation"
    SendAudio     {} -> "/sendAudio"
    SendDocument  {} -> "/sendDocument"
    SendPhoto     {} -> "/sendPhoto"
    SendSticker   {} -> "/sendSticker"
    SendVoice     {} -> "/sendVoice"
    SendVideo     {} -> "/sendVideo"
    SendVideoNote {} -> "/sendVideoNote"
    SendLocation  {} -> "/sendLocation"
    SendVenue     {} -> "/sendVenue"
    SendDice      {} -> "/sendDice"
    SendContact   {} -> "/sendContact"
--TODO: | MediaGroup
--TODO: | Poll

instance Loggable SendRequest where
  toLog sr = case sr of
    SendMessage   body      -> toLog body
    SendVenue     body      -> toLog body
    SendDice      body      -> toLog body
    SendLocation  body      -> toLog body
    SendContact   body      -> toLog body
    SendSticker   id chat   -> mkStickerLog id chat
    SendAnimation id common -> mkMediaLog   id common "Animation"
    SendAudio     id common -> mkMediaLog   id common "Audio"
    SendDocument  id common -> mkMediaLog   id common "Document"
    SendPhoto     id common -> mkMediaLog   id common "Photo"
    SendVoice     id common -> mkMediaLog   id common "Voice"
    SendVideo     id common -> mkMediaLog   id common "Video"
    SendVideoNote id common -> mkMediaLog   id common "VideoNote"
--TODO: | MediaGroup
--TODO: | Poll
    where
      mkMediaLog (FileId id) common srtype = (mkToLog ("Send" <> srtype)
        [(srtype <> " Id", id)] [])
        <> toLog common

      mkStickerLog (FileId id) chat = mkToLog "SendSticker"
        [("Sticker Id", id), ("Chat Id", showt chat)] []

instance HasPriority SendRequest where
  logData sr = logInfo sendInfo >> logDebug (toLog sr)
    where
      sendInfo :: Text
      sendInfo = "Sending message with " <> case sr of
        SendMessage   {} -> "text"
        SendAnimation {} -> "animation"
        SendAudio     {} -> "audio"
        SendDocument  {} -> "document"
        SendPhoto     {} -> "photo"
        SendSticker   {} -> "sticker"
        SendVoice     {} -> "voice"
        SendVideo     {} -> "video"
        SendVideoNote {} -> "video note"
        SendLocation  {} -> "location"
        SendVenue     {} -> "venue"
        SendDice      {} -> "dice"
        SendContact   {} -> "contact"
--TODO: | MediaGroup
--TODO: | Poll

-- SendCommonPart ----------------------------------------------------------

data SendCommonPart = SendCommonPart
  { caption   :: Maybe Text
  , chatId    :: Integer
  , parseMode :: Text
  }

instance Loggable SendCommonPart where
  toLog SendCommonPart {..} = mkToLog ""
    [("Chat Id", showt chatId), ("Parse Mode", parseMode)]
    [("Caption", caption)]

-- SendMessageBody ---------------------------------------------------------

data SendMessageBody = SendMessageBody
  { smText             :: Text
  , smChatId           :: Integer
  , smParseMode        :: Text
  , smReplyToMessageId :: Maybe Integer
  } deriving Generic

instance ToJSON SendMessageBody where toJSON = toJsonDrop

instance Loggable SendMessageBody where
  toLog SendMessageBody {..} = mkToLog "SendMessage:"
    [ ("Text"      , smText)
    , ("Chat Id"   , showt smChatId)
    , ("Parse Mode", smParseMode)
    ] [("Reply Id" , showt <$> smReplyToMessageId)]

-- SendLocationBody --------------------------------------------------------

data SendLocationBody = SendLocationBody
  { slbLongitude      :: Double
  , slbLatitude       :: Double
  , slbChatId         :: Integer
  } deriving Generic

instance ToJSON SendLocationBody where
  toJSON = Aeson.toJsonDrop

instance Loggable SendLocationBody where
  toLog SendLocationBody {..} = mkToLog "SendLocation:"
    [ ("Chat Id"  , showt slbChatId)
    , ("Longitude", showt slbLongitude)
    , ("Latitude" , showt slbLatitude)
    ] []

-- SendVenueBody -----------------------------------------------------------

data SendVenueBody = SendVenueBody
  { svbLongitude      :: Double
  , svbLatitude       :: Double
  , svbChatId         :: Integer
  , svbTitle          :: Text
  , svbAddress        :: Text
  , svbFoursquareId   :: Maybe Text
  , svbFoursquareType :: Maybe Text
  } deriving Generic

instance ToJSON SendVenueBody where
  toJSON = Aeson.toJsonDrop

instance Loggable SendVenueBody where
  toLog SendVenueBody {..} = mkToLog "SendVenue:"
    [ ("Chat Id"  , showt svbChatId)
    , ("Longitude", showt svbLongitude)
    , ("Latitude" , showt svbLatitude)
    , ("Title"    , svbTitle)
    , ("Address"   , svbAddress)
    ]
    [ ("Foursquare Id"  , svbFoursquareId)
    , ("Foursquare Type", svbFoursquareType)
    ]

-- SendContactBody ---------------------------------------------------------

data SendContactBody = SendContactBody
  { scbPhoneNumber :: Text
  , scbFirstName   :: Text
  , scbLastName    :: Maybe Text
  , scbVcard       :: Maybe Text
  , scbChatId      :: Integer
  } deriving Generic

instance ToJSON SendContactBody where
  toJSON = Aeson.toJsonDrop

instance Loggable SendContactBody where
  toLog SendContactBody {..} = mkToLog "SendContact:"
    [ ("Chat Id"     , showt scbChatId)
    , ("Phone Number", scbPhoneNumber)
    , ("First Name"  , scbFirstName)
    ]
    [ ("Last Name"   , scbLastName)
    , ("Vcard"       , scbVcard)
    ]

-- SendDiceBody ------------------------------------------------------------

data SendDiceBody = SendDiceBody
  { sdbEmoji  :: Text
  , sdbValue  :: Integer
  , sdbChatId :: Integer
  } deriving Generic

instance ToJSON SendDiceBody where
  toJSON = Aeson.toJsonDrop

instance Loggable SendDiceBody where
  toLog SendDiceBody {..} = mkToLog "Dice:"
    [ ("Chat Id", showt sdbChatId)
    , ("Emoji"  , sdbEmoji)
    , ("Value"  , showt sdbValue)
    ] []

-- FUNCTIONS ---------------------------------------------------------------

mkRequest :: (TelegramReader env m, ToJSON a) => a -> Text -> m Request
mkRequest x path = do
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
