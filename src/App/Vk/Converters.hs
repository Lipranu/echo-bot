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
  , addSticker
  , mkState
  , mkSaveFile
  , mkUploadFile
  , mkUploadRequests
  , mkSendMessage
  , mkGetUpdates
  ) where

-- IMPORTS -----------------------------------------------------------------

import App.Vk.Internal
import App.Vk.Requests
import App.Vk.Responses

import Control.Monad.State ( MonadState, modify, gets )
import Data.Maybe          ( fromMaybe )
import Data.Text.Extended  ( Text )

import qualified Data.Text.Extended   as Text
import qualified Data.ByteString.Lazy as LBS

-- CLASSES -----------------------------------------------------------------

class Convertible a b | a -> b where
  convert :: a -> b

-- TYPES AND INSTANCES -----------------------------------------------------

data AttachmentsState = AttachmentsState
  { asAttachments :: [Text]
  , asSticker     :: Maybe Integer
  , asPeerId      :: Integer
  }

instance Convertible FileSaved Text where
  convert FileSaved {..} = toAttachment fsType fsOwnerId fsMediaId

instance Convertible AttachmentBody Text where
  convert AttachmentBody {..} = toAttachment aType aOwnerId aId
                             <> case aAccessKey of
                                  Just v  -> "_" <> v
                                  Nothing -> mempty

-- FUNCTIONS ---------------------------------------------------------------

toAttachment :: Text -> Integer -> Integer -> Text
toAttachment t oid mid = t <> Text.showt oid <> "_" <> Text.showt mid

mkGetUpdates :: LongPollServer -> GetUpdates
mkGetUpdates LongPollServer {..} =
  let guKey            = lpsKey
      guTs             = lpsTs
      (guHost, guPath) = Text.span (/='/')
                       $ fromMaybe lpsServer
                       $ Text.stripPrefix "https://" lpsServer
   in GetUpdates {..}

mkState :: Message -> AttachmentsState
mkState Message {..} =
  let asPeerId      = mPeerId
      asAttachments = []
      asSticker     = Nothing
   in AttachmentsState {..}

mkSendMessage :: Message -> AttachmentsState -> Int -> SendMessage
mkSendMessage Message {..} AttachmentsState {..} randomId =
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

mkUploadFile :: UploadServer -> LBS.ByteString -> DocumentBody -> UploadFile
mkUploadFile (UploadServer url) file DocumentBody {..} =
  let ufFile  = LBS.toStrict file
      ufUrl   = url
      ufTitle = dTitle
   in UploadFile {..}

mkUploadRequests :: MonadState AttachmentsState m
                 => DocumentBody
                 -> m (GetFile, GetUploadServer)
mkUploadRequests DocumentBody {..} = do
  id <- gets asPeerId
  return (GetFile dUrl, GetUploadServer "doc" id)

mkSaveFile :: UploadFile -> Text -> SaveFile
mkSaveFile UploadFile {..} file =
  let sfFile  = file
      sfTitle = ufTitle
   in SaveFile {..}

addAttachment :: (Convertible a Text, MonadState AttachmentsState m)
              => a
              -> m ()
addAttachment x = modify $ \as ->
  as { asAttachments = convert x : asAttachments as }

addSticker :: MonadState AttachmentsState m => Integer -> m ()
addSticker id = modify $ \as -> as { asSticker = Just id }
