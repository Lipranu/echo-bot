{-# LANGUAGE OverloadedStrings #-}

module App.Shared
  ( Config (..)
  , HelpText (..)
  , RepeatText (..)
  ) where

import Data.Text.Extended          ( Text )
import App.Shared.Routes -- ( DefaultRepeat (..) )
import Data.Aeson.Extended ( (.:) )
import qualified Data.Aeson.Extended  as Aeson

newtype RepeatText = RepeatText { unRepeatText :: Text }
newtype HelpText   = HelpText { unHelpText :: Text }

data Config = Config
  { cDefaultRepeat :: DefaultRepeat
  , cRepeatText    :: RepeatText
  , cHelpText      :: HelpText
  }

instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "App.Shared.Config" $ \o -> Config
    <$> (DefaultRepeat <$> o .: "default_repeat")
    <*> (RepeatText    <$> o .: "repeat_text")
    <*> (HelpText      <$> o .: "help_text")
