{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE UndecidableInstances            #-}

module App.Telegram ( mkApp, runApp ) where

-- IMPORTS --------------------------------------------------------------------

import App.Telegram.Config

import App.Shared.Config hiding ( Config )
import App.Shared
import App.Shared.Routes

import Infrastructure.Has
import Infrastructure.Logger    hiding ( Config, Priority (..) )
import Infrastructure.Requester

import qualified App.Shared.Config     as Shared
import qualified Infrastructure.Logger as Logger

import Control.Applicative         ( (<|>) )
import Control.Monad.Reader        ( ReaderT (..) )
import Data.Aeson.Extended         ( (.:) )
import Data.Text.Encoding.Extended ( encodeUtf8, encodeShowUtf8 )
import Data.Text.Extended          ( Text )
import Data.IORef           ( IORef )

import qualified Data.Aeson.Extended  as Aeson
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Extended   as Text
import qualified Data.Text.IO         as TextIO
import qualified Network.HTTP.Client  as HTTP

-- TYPES AND INSTANCES -----------------------------------------------------

data Env = Env
  { envToken         :: Token
  , envDefaultRepeat :: DefaultRepeat
  , envRepetitions   :: IORef Repetitions
  , envLogger        :: Logger (App Env)
  , envLock          :: Lock
  , envRequester     :: Requester (App Env)
  , envHelpText      :: HelpText
  , envRepeatText    :: RepeatText
  }

instance Has Lock                  Env where getter = envLock
instance Has (Logger (App Env))    Env where getter = envLogger
instance Has (Requester (App Env)) Env where getter = envRequester
instance Has Token                 Env where getter = envToken
instance Has DefaultRepeat         Env where getter = envDefaultRepeat
instance Has HelpText              Env where getter = envHelpText
instance Has RepeatText            Env where getter = envRepeatText
instance Has (IORef Repetitions)   Env where getter = envRepetitions

-- Response ----------------------------------------------------------------

data Response a
  = Succes a
  | Error Integer Text

instance Aeson.FromJSON a => Aeson.FromJSON (Response a) where
  parseJSON = Aeson.withObject "App.Vk.Response" $ \o ->
        Succes <$> o .: "result"
    <|> Error  <$> o .: "error_code"
               <*> o .: "description"

instance Loggable a => Loggable (Response a) where
  toLog (Succes x) = toLog x

  toLog (Error code description)
    = "An error occurred as a result of the request\n\
    \ | Error Code: "        <> Text.showt code <> "\n\
    \ | Error Description: " <> description

-- GetUpdates --------------------------------------------------------------

newtype GetUpdates = GetUpdates (Maybe Integer)

instance (TelegramReader r m, Monad m) => ToRequest m GetUpdates where
  toRequest (GetUpdates Nothing) = do
    token <- obtain
    return $
      HTTP.urlEncodedBody defaultGetUpdatesBody $
      defaultRequest
      { HTTP.path = defaultPath token <> "/getUpdates"
      , HTTP.method = "GET"
      }

  toRequest (GetUpdates (Just n)) = do
    token <- obtain
    return $
      HTTP.urlEncodedBody mkBody $
      defaultRequest
      { HTTP.path = defaultPath token <> "/getUpdates" }
    where mkBody = ("offset" , encodeShowUtf8 $ n + 1)
                 : defaultGetUpdatesBody

instance Loggable GetUpdates where
  toLog (GetUpdates Nothing)
    = "GetUpdates request without offset"
  toLog (GetUpdates (Just n))
    = "GetUpdates request with offset: " <> Text.showt (n + 1)

-- Update ------------------------------------------------------------------

newtype Update = Post Integer

instance Aeson.FromJSON Update where
  parseJSON = Aeson.withObject "App.Vk.Update" $ \o -> Post
    <$> o .: "update_id"

instance Loggable [Update] where
  toLog v = "Updates resived: " <> Text.showt (length v)

instance Loggable Update where
  toLog (Post i) = "Proccess post with id: " <> Text.showt i

-- FUNCTIONS ---------------------------------------------------------------

app :: App Env ()
app = start

mkApp Config {..} Shared.Config {..} logger lock ref manager =
  let envLock          = lock
      envLogger        = mkLogger logger "Telegram"
      envToken         = cToken
      envRequester     = mkRequester manager
      envDefaultRepeat = cDefaultRepeat
      envRepeatText    = cRepeatText
      envHelpText      = cHelpText
      envRepetitions   = ref
   in Env {..}

runApp :: Env -> IO ()
runApp = runReaderT (unApp app)

--getUpdates :: GetUpdates -> App ()
--getUpdates gu = do
--  logInfo gu
--  result <- requestAndDecode gu
--  case result of
--    Result (Succes v) -> do
--      logInfo v
--      proccessUpdates v
--    error -> do
--      logError error
--      logError ("Application shut down" :: Text)

--proccessUpdates :: [Update] -> App ()
--proccessUpdates xs = do
--  x <- foldM proccessUpdate Nothing xs
--  getUpdates $ GetUpdates x
--
--proccessUpdate :: Maybe Integer -> Update -> App (Maybe Integer)
--proccessUpdate current p@(Post id) = do
--  logDebug p
--  return (max current $ Just id)

defaultRequest :: HTTP.Request
defaultRequest = HTTP.defaultRequest
  { HTTP.host = "api.telegram.org"
  , HTTP.method = "POST"
  , HTTP.secure = True
  , HTTP.port   = 443
  }

defaultPath :: Token -> BS.ByteString
defaultPath token = "/bot" <> encodeUtf8 (unToken token)

defaultGetUpdatesBody :: [(BS.ByteString, BS.ByteString)]
defaultGetUpdatesBody =
  let list :: [Text]
      list = ["message", "edited_channel_post", "callback_query"]
   in [ ("timeout", "25")
      , ("allowed_updates", LBS.toStrict $ Aeson.encode list)
      ]
