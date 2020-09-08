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

import Data.Maybe                  ( fromMaybe )
import Data.Text.Extended          ( Text )

import qualified Data.Text.Extended as Text

-- CLASSES -----------------------------------------------------------------

class Convertible a b | b -> a, a -> b where
  convert :: a -> b

-- TYPES AND INSTANCES -----------------------------------------------------

instance Convertible LongPollServer GetUpdates where
  convert LongPollServer {..} =
    let guKey            = lpsKey
        guTs             = lpsTs
        (guHost, guPath) = Text.span (/='/')
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
    let smPeerId      = mPeerId
        smRandomId    = randomId
        smMessage     = mMessage
        smLatitude    = mLatitude
        smLongitude   = mLongitude
        smSticker     = asSticker
        smAttachments = case asAttachments of
          [] -> Nothing
          xs -> Just $ Text.intercalate "," $ reverse xs
     in SendMessage {..}
