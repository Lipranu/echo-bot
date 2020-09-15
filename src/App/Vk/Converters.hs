{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}

module App.Vk.Converters
  ( module App.Vk.Requests
  , module App.Vk.Responses
  , module App.Vk.Internal

  , Convertible (..)
  , AttachmentsState (..)

  , addAttachment
  , addSticker
  , mkGetFile
  , mkGetUpdates
  , mkGetUploadServer
  , mkSaveFile
  , mkSendMessage
  , mkRepeatReply
  , mkState
  , mkUploadFile
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
  convert FileSaved {..}
    = toAttachment fsType fsOwnerId fsMediaId

instance Convertible AttachmentBody Text where
  convert AttachmentBody {..}
    = toAttachmentWithKey aType aOwnerId aId aAccessKey

instance Convertible WallBody Text where
  convert WallBody {..}
    = toAttachmentWithKey wType wToId wId wAccessKey

-- FUNCTIONS ---------------------------------------------------------------

toAttachment :: Text -> Integer -> Integer -> Text
toAttachment t oid mid = t <> Text.showt oid <> "_" <> Text.showt mid

toAttachmentWithKey :: Text -> Integer -> Integer -> Maybe Text -> Text
toAttachmentWithKey t oid mid key = toAttachment t oid mid <> case key of
  Just v  -> "_" <> v
  Nothing -> ""

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

mkSendMessage :: Message -> AttachmentsState -> Int -> Int -> SendMessage
mkSendMessage Message {..} AttachmentsState {..} currentRepeat randomId =
  let smPeerId      = mPeerId
      smRandomId    = randomId
      smMessage     = mMessage
      smLatitude    = mLatitude
      smLongitude   = mLongitude
      smSticker     = asSticker
      smKeyboard    = mkKeyboard currentRepeat
      smAttachments = case asAttachments of
        [] -> Nothing
        xs -> Just $ Text.intercalate "," $ reverse xs
   in SendMessage {..}

mkRepeatReply :: Message -> Int -> Int -> SendMessage
mkRepeatReply Message {..} repeat randomId =
  let smPeerId      = mPeerId
      smRandomId    = randomId
      smMessage     = Just $ "Repeat count set to: " <> Text.showt repeat
      smLatitude    = Nothing
      smLongitude   = Nothing
      smSticker     = Nothing
      smKeyboard    = mkKeyboard repeat
      smAttachments = Nothing
   in SendMessage {..}

mkKeyboard :: Int -> Keyboard
mkKeyboard currentRepeat =
  let kOneTime = False
      kButtons = [[helpButton, repeatButton], indexButtons currentRepeat]
      kInline  = False
   in Keyboard {..}

helpButton :: Button
helpButton =
  let bColor  = "primary"
      bAction = helpAction
   in Button {..}

helpAction :: Action
helpAction =
  let abType    = "text"
      abLabel   = "Help"
      abPayload = "101"
   in Action {..}

repeatButton :: Button
repeatButton =
  let bColor  = "primary"
      bAction = repeatAction
   in Button {..}

repeatAction :: Action
repeatAction =
  let abType    = "text"
      abLabel   = "Repeat"
      abPayload = "102"
   in Action {..}

indexButtons :: Int -> [Button]
indexButtons currentRepeat =
  [ indexButton 1 currentRepeat
  , indexButton 2 currentRepeat
  , indexButton 3 currentRepeat
  , indexButton 4 currentRepeat
  , indexButton 5 currentRepeat
  ]

indexButton :: Int -> Int -> Button
indexButton index currentRepeat =
  let bAction = indexAction $ Text.showt index
      bColor  | index == currentRepeat = "positive"
              | otherwise              = "secondary"
   in Button {..}

indexAction :: Text -> Action
indexAction index =
  let abType    = "text"
      abLabel   = index
      abPayload = "20" <> index
   in Action {..}

mkUploadFile :: DocumentBody -> UploadServer -> RawFile -> UploadFile
mkUploadFile DocumentBody {..} (UploadServer url) (RawFile file) =
  let ufFile  = LBS.toStrict file
      ufUrl   = url
      ufTitle = dTitle
   in UploadFile {..}

mkGetFile :: DocumentBody -> GetFile
mkGetFile DocumentBody {..} = GetFile dUrl

mkGetUploadServer :: MonadState AttachmentsState m => m GetUploadServer
mkGetUploadServer = GetUploadServer "doc" <$> gets asPeerId

mkSaveFile :: UploadFile -> FileUploaded -> SaveFile
mkSaveFile UploadFile {..} (FileUploaded file) =
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
