{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}

module App.Vk.Converters
  ( AttachmentsState (..)
  , Command (..)
  , Context (..)
  , Convertible (..)
  , mkCommandReply
  , mkCommandText
  , mkContext
  , mkGetFile
  , mkGetName
  , mkGetUpdates
  , mkGetUploadServer
  , mkSaveFile
  , mkSendMessage
  , mkState
  , mkUploadFile
  ) where

-- IMPORTS -----------------------------------------------------------------

import App.Vk.Requests
import App.Vk.Responses

import Infrastructure.Logger ( Loggable (..), HasPriority (..), logInfo )

import Control.Monad.State   ( MonadState, gets )
import Data.Maybe            ( fromMaybe )
import Data.Text.Extended    ( Text, showt )

import qualified Data.Text.Extended   as Text
import qualified Data.ByteString.Lazy as LBS

-- CLASSES -----------------------------------------------------------------

class Convertible a b | a -> b where
  convert :: a -> b

-- TYPES AND INSTANCES -----------------------------------------------------

data Context
  = Private
  | Chat

data Command
  = Help
  | Repeat
  | NewRepeat Int

instance Loggable Command where
  toLog Help          = "Performing Help Command"
  toLog Repeat        = "Performing Repeat Command"
  toLog (NewRepeat i) = "Setting new repeat count: "  <> showt i

instance HasPriority Command where
  logData = logInfo . toLog

data AttachmentsState = AttachmentsState
  { asAttachments :: [Text]
  , asSticker     :: Maybe Integer
  , asPeerId      :: Integer
  , asContext     :: Context
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
mkState message =
  let asPeerId      = mPeerId message
      asAttachments = []
      asSticker     = Nothing
      asContext     = mkContext message
   in AttachmentsState {..}

mkContext :: Message -> Context
mkContext Message {..}
  | mPeerId == mFromId = Private
  | otherwise          = Chat

mkGetName :: Message -> GetName
mkGetName Message {..} = GetName mFromId

mkSendMessage :: Message -> AttachmentsState -> Int -> Int -> SendMessage
mkSendMessage Message {..} AttachmentsState {..} currentRepeat randomId =
  let smPeerId      = mPeerId
      smRandomId    = randomId
      smMessage     = mMessage
      smLatitude    = mLatitude
      smLongitude   = mLongitude
      smSticker     = asSticker
      smKeyboard    = Just $ mkKeyboard currentRepeat
      smAttachments = case asAttachments of
        [] -> Nothing
        xs -> Just $ Text.intercalate "," $ reverse xs
   in SendMessage {..}

mkCommandReply :: Message -> Text -> Int -> SendMessage
mkCommandReply Message {..} text randomId =
  let smPeerId      = mPeerId
      smRandomId    = randomId
      smLatitude    = Nothing
      smLongitude   = Nothing
      smSticker     = Nothing
      smKeyboard    = Nothing
      smAttachments = Nothing
      smMessage     = Just text
   in SendMessage {..}

mkCommandText :: Message -> Context -> Maybe UserName -> Text -> Text
mkCommandText _ Private _ text = text
mkCommandText Message {..} Chat un text
  = "@id" <> Text.showt mFromId <> case un of
    Nothing           -> ", " <> text
    Just (UserName n) -> " (" <> n <> "), " <> text

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
