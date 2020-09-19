{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App.Shared
  ( Config (..)
  , HelpText (..)
  , RepeatText (..)
  , App (..)
  ) where

import Infrastructure.Logger    ( MonadLogger (..), MonadTime (..) )
import Infrastructure.Requester ( MonadRequester (..) )
import App.Shared.Routes

import Control.Monad.Catch      ( MonadCatch, MonadThrow )
import Control.Monad.Reader     ( MonadReader, ReaderT (..) )
import Control.Monad.IO.Class   ( MonadIO, liftIO )
import Data.Text.Extended       ( Text )
import Data.Aeson.Extended      ( FromJSON (..), (.:), withObject )
import Data.Time                ( getCurrentTime )
import Network.HTTP.Client      ( httpLbs )

import qualified Data.Text.IO as TextIO

newtype RepeatText = RepeatText { unRepeatText :: Text }
newtype HelpText   = HelpText   { unHelpText   :: Text }

data Config = Config
  { cDefaultRepeat :: DefaultRepeat
  , cRepeatText    :: RepeatText
  , cHelpText      :: HelpText
  }

instance FromJSON Config where
  parseJSON = withObject "App.Shared.Config" $ \o -> Config
    <$> (DefaultRepeat <$> o .: "default_repeat")
    <*> (RepeatText    <$> o .: "repeat_text")
    <*> (HelpText      <$> o .: "help_text")

newtype App r a = App { unApp :: ReaderT r IO a }
  deriving ( Applicative
           , Functor
           , Monad
           , MonadCatch
           , MonadIO
           , MonadReader r
           , MonadThrow
           )

instance MonadLogger (App r) where
  logConsole   = liftIO . TextIO.putStr
  logFile path = liftIO . TextIO.appendFile path

instance MonadTime (App r) where
  getTime = liftIO getCurrentTime

instance MonadRequester (App r) where
  requester manager req = liftIO $ httpLbs req manager
