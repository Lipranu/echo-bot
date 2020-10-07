{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE LambdaCase             #-}

module App.Vk.Converters
  ( AttachmentsState (..)
  , Command (..)
  , Context (..)
  , ToAttachment (..)
  , UploadRequests (..)
  , ToUploadRequests (..)
  , docToPhoto
  , mkCommandReply
  , mkCommandReply'
  , mkCommandText
  , mkContext
  , mkKeyboard
  , mkGetName
  , mkGetName'
  , mkNotification
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

import Control.Monad.State ( MonadState )
import Data.Maybe          ( fromMaybe )
import Data.Text.Extended  ( Text )

import qualified Data.Text.Extended   as Text
import qualified Data.ByteString.Lazy as LBS

-- CLASSES -----------------------------------------------------------------

class ToAttachment a where
  toAttachment :: a -> Text

class ToUploadRequests m a b | a -> b where
  toUploadRequests :: a -> m (UploadRequests b)

-- TYPES AND INSTANCES -----------------------------------------------------

data AttachmentsState = AttachmentsState
  { asAttachments :: [Text]
  , asSticker     :: Maybe Integer
  , asPeerId      :: PeerId
  , asFromId      :: FromId
  , asMessageId   :: MessageId
  , asContext     :: Context
  }

instance Has Context AttachmentsState where
  getter = asContext

instance Has PeerId AttachmentsState where
  getter = asPeerId

instance Has FromId AttachmentsState where
  getter = asFromId

instance Has MessageId AttachmentsState where
  getter = asMessageId

instance ToAttachment FileSaved where
  toAttachment FileSaved {..}
    = mkAttachment fsType fsOwnerId fsMediaId fsAccessKey

instance ToAttachment PhotoSaved where
  toAttachment PhotoSaved {..}
    = mkAttachment "photo" psOwnerId psMediaId psAccessKey

instance ToAttachment AttachmentBody where
  toAttachment AttachmentBody {..}
    = mkAttachment aType aOwnerId aId aAccessKey

instance ToAttachment PhotoBody where
  toAttachment PhotoBody {..}
    = mkAttachment "photo" pbOwnerId pbId pbAccessKey

data UploadRequests a = UploadRequests
  { getUploadServer :: GetUploadServer
  , getFile         :: GetFile
  , uploadFile      :: UploadServer -> LBS.ByteString -> UploadFile
  , saveFile        :: FileUploaded -> SaveFile
  }

instance Applicative m => ToUploadRequests m PhotoBody PhotoSaved where
  toUploadRequests PhotoBody {..} =
    let getUploadServer = PhotoUploadServer
        getFile         = GetFile pbUrl
        uploadFile      = mkUploadFile "photo" pbTitle
        saveFile        = mkSaveFile pbTitle
     in pure UploadRequests {..}

instance (MonadState s m, Has PeerId s)
  => ToUploadRequests m DocumentBody FileSaved where
  toUploadRequests DocumentBody {..} = do
    let getFile         = GetFile dbUrl
        uploadFile      = mkUploadFile dbType dbTitle
        saveFile        = mkSaveFile dbTitle
    getUploadServer <- mkFileUploadServer dbType
    pure UploadRequests {..}

instance (MonadState s m, Has PeerId s)
  => ToUploadRequests m AudioMessageBody FileSaved where
  toUploadRequests AudioMessageBody {..} = do
    let getFile         = GetFile ambUrl
        uploadFile      = mkUploadFile "audio_message" ambTitle
        saveFile        = mkSaveFile ambTitle
    getUploadServer <- mkFileUploadServer "audio_message"
    pure UploadRequests {..}

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
  let asPeerId      = mPeerId message
      asFromId      = mFromId message
      asAttachments = []
      asSticker     = Nothing
      asMessageId   = mId message
      asContext     = mkContext message
   in AttachmentsState {..}

mkContext :: Message -> Context
mkContext Message {..}
  | peerId == fromId = Private
  | otherwise        = Chat
  where peerId = unPeerId mPeerId
        fromId = unFromId mFromId

mkGetName :: FromId -> GetName
mkGetName = GetName

mkGetName' :: (Has FromId s, MonadState s m) => m GetName
mkGetName' = GetName <$> grab

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

mkCommandReply' :: ( Has Context s
                   , Has FromId s
                   , Has PeerId s
                   , MonadState s m
                   )
                => Text
                -> Maybe UserName
                -> m (Int -> SendMessage)
mkCommandReply' text user = do
  let smLatitude    = Nothing
      smLongitude   = Nothing
      smStickerId   = Nothing
      smKeyboard    = Nothing
      smAttachments = Nothing
      smReplyId     = Nothing
      smForwardsId  = Nothing
  smPeerId  <- grab
  smMessage <- Just <$> mkAppeal text user
  pure $ \smRandomId -> SendMessage {..}

mkNotification :: Context
               -> Maybe UserName
               -> MessageId
               -> FromId
               -> PeerId
               -> Text
               -> Int
               -> SendMessage
mkNotification context user messageId fromId peerId mType randomId =
  let smPeerId      = peerId
      smRandomId    = randomId
      smLatitude    = Nothing
      smLongitude   = Nothing
      smStickerId   = Nothing
      smKeyboard    = Nothing
      smAttachments = Nothing
      smForwardsId  = Nothing
      smMessage     = Just $ mkCommandText context text user fromId
      text          = "Can't send message of type: " <> mType
      smReplyId     = case context of
        Chat    -> Nothing
        Private -> Just $ unMessageId messageId
   in SendMessage {..}

mkCommandText :: Context -> Text -> Maybe UserName -> FromId -> Text
mkCommandText Private text _ _ = text
mkCommandText Chat text user fromId
  = "@id" <> Text.showt (unFromId fromId) <> case user of
    Nothing           -> ", " <> text
    Just (UserName n) -> " (" <> n <> "), " <> text

mkAppeal :: (Has Context s, Has FromId s, MonadState s m)
              => Text
              -> Maybe UserName
              -> m Text
mkAppeal text user = grab >>= \case
  Private -> pure text
  Chat    -> do
    fromId <- Text.showt . unFromId <$> grab
    pure $ "@id" <> fromId <> case user of
      Nothing -> ", " <> text
      Just name -> " (" <> unFirstName name <> "), " <> text

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

mkFileUploadServer :: (MonadState s m, Has PeerId s)
                   => Text
                   -> m GetUploadServer
mkFileUploadServer dType = FileUploadServer dType <$> grab

mkUploadFile :: Text -> Text -> UploadServer -> LBS.ByteString -> UploadFile
mkUploadFile ufType ufTitle (UploadServer ufUrl) file =
  let ufFile = LBS.toStrict file
   in UploadFile {..}

mkSaveFile :: Text -> FileUploaded -> SaveFile
mkSaveFile title (DocumentUploaded file) = SaveDocument title file
mkSaveFile title (PhotoUploaded server hash photo) =
  SavePhoto title server hash photo

docToPhoto :: DocumentBody -> PhotoBody
docToPhoto DocumentBody {..} =
  let pbUrl       = dbUrl
      pbTitle     = dbTitle
      pbId        = dbId
      pbOwnerId   = dbOwnerId
      pbAccessKey = dbAccessKey
   in PhotoBody {..}
