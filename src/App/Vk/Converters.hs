{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}

module App.Vk.Converters
  ( module App.Vk.Requests
  , module App.Vk.Responses
  , module App.Vk.Internal

  , Convertible (..)
  , AttachmentsState (..)
  ) where

-- IMPORTS -----------------------------------------------------------------

import App.Vk.Internal
import App.Vk.Requests
import App.Vk.Responses

import Data.Bifunctor              ( bimap )
import Data.Maybe                  ( fromMaybe )
import Data.Text.Extended          ( Text )
import Data.Text.Encoding.Extended ( encodeUtf8, encodeShowUtf8)

import qualified Data.Text.Extended as Text

-- CLASSES -----------------------------------------------------------------

class Convertible a b | b -> a, a -> b where
  convert :: a -> b

-- TYPES AND INSTANCES -----------------------------------------------------

instance Convertible LongPollServer GetUpdates where
  convert LongPollServer {..} =
    let guKey            = encodeUtf8 lpsKey
        guTs             = encodeUtf8 lpsTs
        (guHost, guPath) = bimap encodeUtf8 encodeUtf8
                         $ Text.span (/='/')
                         $ fromMaybe lpsServer
                         $ Text.stripPrefix "https://" lpsServer
     in GetUpdates {..}

data AttachmentsState = AttachmentsState
  { asAttachments :: [Text]
  , asSticker     :: Maybe Integer
  , asPeerId      :: Integer
  }

instance Convertible Message (Int -> AttachmentsState -> SendMessage) where
  convert Message {..} randomId AttachmentsState {..} =
    let smPeerId      = encodeShowUtf8     mPeerId
        smRandomId    = encodeShowUtf8     randomId
        smMessage     = encodeUtf8     <$> mMessage
        smLatitude    = encodeShowUtf8 <$> mLatitude
        smLongitude   = encodeShowUtf8 <$> mLongitude
        smSticker     = encodeShowUtf8 <$> asSticker
        smAttachments = case asAttachments of
          [] -> Nothing
          xs -> Just $ encodeUtf8 $ Text.intercalate "," $ reverse xs
     in SendMessage {..}
