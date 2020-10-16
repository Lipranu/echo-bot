{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}

module App.Telegram.Converters ( mkSendRequest ) where

-- IMPORTS -----------------------------------------------------------------

import App.Telegram.Responses
import App.Telegram.Requests

import Infrastructure.Requester

import Data.Coerce ( coerce )
import Data.Text   ( Text )
import Data.Maybe  ( fromMaybe )

-- FUNCTIONS ---------------------------------------------------------------

mkSendRequest :: MessageBody -> Maybe Text -> SendRequest
mkSendRequest MessageBody {..} text = case mbType of
  TextMessage  -> SendMessage   mkSendMessageBody
  Sticker   id -> SendSticker   (coerce id) $ mkCommonPart Nothing
  Animation id -> SendAnimation (coerce id) $ mkCommonPart text
  Audio     id -> SendAudio     (coerce id) $ mkCommonPart text
  Document  id -> SendDocument  (coerce id) $ mkCommonPart text
  Photo     id -> SendPhoto     (coerce id) $ mkCommonPart text
  Video     id -> SendVideo     (coerce id) $ mkCommonPart text
  VideoNote id -> SendVideoNote (coerce id) $ mkCommonPart text
  Voice     id -> SendVoice     (coerce id) $ mkCommonPart text
  where
    mkSendMessageBody = SendMessageBody
      { smText             = fromMaybe "" text
      , smChatId           = mbChatId
      , smParseMode        = "HTML"
      , smReplyToMessageId = Nothing
      }

    mkCommonPart mText = SendCommonPart
      { caption   = mText
      , chatId    = mbChatId
      , parseMode = "HTML"
      }
