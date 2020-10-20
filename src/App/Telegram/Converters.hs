{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}

module App.Telegram.Converters ( mkSendRequest ) where

-- IMPORTS -----------------------------------------------------------------

import App.Telegram.Responses
import App.Telegram.Requests

import Data.Text   ( Text )
import Data.Maybe  ( fromMaybe )

-- FUNCTIONS ---------------------------------------------------------------

mkSendRequest :: MessageBody -> Maybe Text -> SendRequest
mkSendRequest MessageBody {..} text = case mbType of
  TextMessage    -> SendMessage   mkSendMessageBody
  Venue     body -> SendVenue     $ mkSendVenueBody    body
  Location  body -> SendLocation  $ mkSendLocationBody body
  Contact   body -> SendContact   $ mkSendContactBody  body
  Dice      body -> SendDice      $ mkSendDiceBody     body
  Sticker   id   -> SendSticker   id mbChatId
  Animation id   -> SendAnimation id $ mkCommonPart text
  Audio     id   -> SendAudio     id $ mkCommonPart text
  Document  id   -> SendDocument  id $ mkCommonPart text
  Photo     id   -> SendPhoto     id $ mkCommonPart text
  Video     id   -> SendVideo     id $ mkCommonPart text
  VideoNote id   -> SendVideoNote id $ mkCommonPart text
  Voice     id   -> SendVoice     id $ mkCommonPart text
--TODO: | MediaGroup
--TODO: | Poll
  where
    mkSendMessageBody = SendMessageBody
      { smText             = fromMaybe "" text
      , smChatId           = mbChatId
      , smParseMode        = "HTML"
      , smReplyToMessageId = Nothing
      }

    mkSendLocationBody LocationBody {..} = SendLocationBody
      { slbChatId    = mbChatId
      , slbLongitude = longitude
      , slbLatitude  = latitude
      }

    mkSendVenueBody VenueBody {..} = SendVenueBody
      { svbChatId         = mbChatId
      , svbLongitude      = longitude vbLocation
      , svbLatitude       = latitude  vbLocation
      , svbAddress        = vbAddress
      , svbTitle          = vbTitle
      , svbFoursquareId   = vbFoursquareId
      , svbFoursquareType = vbFoursquareType
      }

    mkSendContactBody ContactBody {..} = SendContactBody
      { scbPhoneNumber = cbPhoneNumber
      , scbFirstName   = cbFirstName
      , scbLastName    = cbLastName
      , scbVcard       = cbVcard
      , scbChatId      = mbChatId
      }

    mkSendDiceBody DiceBody {..} = SendDiceBody
      { sdbChatId = mbChatId
      , sdbEmoji  = dbEmoji
      , sdbValue  = dbValue
      }

    mkCommonPart mText = SendCommonPart
      { caption   = mText
      , chatId    = mbChatId
      , parseMode = "HTML"
      }
