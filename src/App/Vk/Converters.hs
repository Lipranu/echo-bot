{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}

module App.Vk.Converters
  ( AttachmentsState (..)
  , Command (..)
  , Context (..)
  , ToAttachment (..)
  , UploadRequests (..)
  , ToUploadRequests (..)
  , mkCommandReply
  , mkCommandText
  , mkContext
  , mkKeyboard
  , mkGetName
  , mkGetUpdates
  , mkSaveFile
  , mkSendMessage
  , mkState
  , mkUploadFile
  ) where

-- IMPORTS -----------------------------------------------------------------

import App.Vk.Requests
import App.Vk.Responses

import Infrastructure.Has

import Data.Maybe            ( fromMaybe )
import Data.Text.Extended    ( Text )

import qualified Data.Text.Extended   as Text
import qualified Data.ByteString.Lazy as LBS

-- CLASSES -----------------------------------------------------------------

class ToAttachment a where
  toAttachment :: a -> Text

class ToUploadRequests a b | a -> b where
  toUploadRequests :: a -> UploadRequests b

-- TYPES AND INSTANCES -----------------------------------------------------

data Context
  = Private
  | Chat

data AttachmentsState = AttachmentsState
  { asAttachments :: [Text]
  , asSticker     :: Maybe Integer
  , asPeerId      :: PeerId
  , asContext     :: Context
  }

instance Has Context AttachmentsState where
  getter = asContext

instance Has PeerId AttachmentsState where
  getter = asPeerId

instance ToAttachment FileSaved where
  toAttachment FileSaved {..}
    = mkAttachment fsType fsOwnerId fsMediaId fsAccessKey

instance ToAttachment PhotoSaved where
  toAttachment PhotoSaved {..}
    = mkAttachment "photo" psOwnerId psMediaId psAccessKey

instance ToAttachment AttachmentBody where
  toAttachment AttachmentBody {..}
    = mkAttachment aType aOwnerId aId aAccessKey

instance ToAttachment WallBody where
  toAttachment WallBody {..}
    = mkAttachment wType wToId wId wAccessKey

instance ToAttachment PhotoBody where
  toAttachment PhotoBody {..}
    = mkAttachment "photo" pbOwnerId pbId pbAccessKey

data UploadRequests a = UploadRequests
  { getUploadServer :: GetUploadServer
  , getFile         :: GetFile
  , uploadFile      :: UploadServer -> LBS.ByteString -> UploadFile
  , saveFile        :: FileUploaded -> SaveFile
  }

instance ToUploadRequests PhotoBody PhotoSaved where
  toUploadRequests PhotoBody {..} =
    let getUploadServer = PhotoUploadServer
        getFile         = GetFile pbUrl
        uploadFile      = mkUploadFile "photo" title
        saveFile        = mkSaveFile title
        title           = snd $ Text.breakOnEnd "/" pbUrl
     in UploadRequests {..}

instance ToUploadRequests DocumentBody FileSaved where
  toUploadRequests DocumentBody {..} =
    let getUploadServer = FileUploadServer "doc"
        getFile         = GetFile dUrl
        uploadFile      = mkUploadFile "doc" dTitle
        saveFile        = mkSaveFile dTitle
     in UploadRequests {..}

-- FUNCTIONS ---------------------------------------------------------------

mkAttachment :: Text -> Integer -> Integer -> Maybe Text -> Text
mkAttachment t oid mid key = t
  <> Text.showt oid
  <> "_"
  <> Text.showt mid
  <> case key of
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
  let asPeerId      = PeerId $ mPeerId message
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

mkSendMessage :: Message
              -> AttachmentsState
              -> Maybe Keyboard
              -> Int
              -> SendMessage
mkSendMessage Message {..} AttachmentsState {..} keyboard randomId =
  let smPeerId      = mPeerId
      smRandomId    = randomId
      smMessage     = mMessage
      smLatitude    = mLatitude
      smLongitude   = mLongitude
      smStickerId   = asSticker
      smKeyboard    = keyboard
      smReplyId     = mReplyId
      smForwardsId  = listToText $ Text.showt <$> mForwardsId
      smAttachments = listToText $ reverse asAttachments
      listToText [] = Nothing
      listToText xs = Just $ Text.intercalate "," xs
   in SendMessage {..}

mkCommandReply :: Message -> Text -> Int -> SendMessage
mkCommandReply Message {..} text randomId =
  let smPeerId      = mPeerId
      smRandomId    = randomId
      smLatitude    = Nothing
      smLongitude   = Nothing
      smStickerId   = Nothing
      smKeyboard    = Nothing
      smAttachments = Nothing
      smReplyId     = Nothing
      smForwardsId  = Nothing
      smMessage     = Just text
   in SendMessage {..}

mkCommandText :: Message -> Context -> Maybe UserName -> Text -> Text
mkCommandText _ Private _ text = text
mkCommandText Message {..} Chat un text
  = "@id" <> Text.showt mFromId <> case un of
    Nothing           -> ", " <> text
    Just (UserName n) -> " (" <> n <> "), " <> text

mkKeyboard :: Maybe Keyboard
mkKeyboard =
  let kOneTime = False
      kButtons = [[helpButton, repeatButton], indexButtons]
      kInline  = False
   in Just Keyboard {..}

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

indexButtons :: [Button]
indexButtons =
  [ indexButton 1
  , indexButton 2
  , indexButton 3
  , indexButton 4
  , indexButton 5
  ]

indexButton :: Int -> Button
indexButton index =
  let bAction = indexAction $ Text.showt index
      bColor  = "secondary"
   in Button {..}

indexAction :: Text -> Action
indexAction index =
  let abType    = "text"
      abLabel   = index
      abPayload = "20" <> index
   in Action {..}

mkUploadFile :: Text -> Text -> UploadServer -> LBS.ByteString -> UploadFile
mkUploadFile ufType ufTitle (UploadServer ufUrl) file =
  let ufFile = LBS.toStrict file
   in UploadFile {..}

mkSaveFile :: Text -> FileUploaded -> SaveFile
mkSaveFile title (DocumentUploaded file) = SaveDocument title file
mkSaveFile title (PhotoUploaded server hash photo) =
  SavePhoto title server hash photo
