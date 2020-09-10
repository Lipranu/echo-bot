{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE FlexibleContexts       #-}

module App.Vk.Converters
  ( module App.Vk.Requests
  , module App.Vk.Responses
  , module App.Vk.Internal

  , Convertible (..)
  , AttachmentsState (..)

  , addAttachment
  ) where

-- IMPORTS -----------------------------------------------------------------

import App.Vk.Internal
import App.Vk.Requests
import App.Vk.Responses

import Control.Monad.State ( MonadState, modify )
import Data.Maybe          ( fromMaybe )
import Data.Text.Extended  ( Text )

import qualified Data.Text.Extended   as Text
import qualified Data.ByteString.Lazy as LBS

-- CLASSES -----------------------------------------------------------------

class Convertible a b | a -> b where
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

instance Convertible FileSaved Text where
  convert FileSaved {..} = fsType
                        <> Text.showt fsOwnerId
                        <> "_"
                        <> Text.showt fsMediaId

instance Convertible AttachmentBody Text where
  convert AttachmentBody {..} = aType
                             <> Text.showt aOwnerId
                             <> "_"
                             <> Text.showt aId
                             <> case aAccessKey of
                                  Just v -> "_" <> v
                                  Nothing -> mempty

instance Convertible UploadServer
  (LBS.ByteString -> DocumentBody -> UploadFile) where
  convert (UploadServer url) file DocumentBody {..} =
    let ufFile  = LBS.toStrict file
        ufUrl   = url
        ufTitle = dTitle
     in UploadFile {..}

instance Convertible UploadFile (Text -> SaveFile) where
  convert UploadFile {..} file =
    let sfFile  = file
        sfTitle = ufTitle
     in SaveFile {..}

-- FUNCTIONS ---------------------------------------------------------------

addAttachment :: (Convertible a Text, MonadState AttachmentsState m)
              => a
              -> m ()
addAttachment x = modify $ \as ->
  as { asAttachments = convert x : asAttachments as }
