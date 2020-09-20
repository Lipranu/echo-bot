{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Shared.Config
  ( Config (..)
  , DefaultRepeat (..)
  , HelpText (..)
  , RepeatText (..)
  , SharedReader
  ) where

-- IMPORTS -----------------------------------------------------------------

import Internal             ( Has )

import Control.Monad.Reader ( MonadReader )
import Data.Aeson           ( FromJSON (..), (.:), withObject )
import Data.Text            ( Text )

-- TYPES AND INSTANCES -----------------------------------------------------

type SharedReader r m = (MonadReader r m, Has HelpText r, Has RepeatText r)

newtype DefaultRepeat = DefaultRepeat { unDefaultRepeat :: Int }
newtype RepeatText    = RepeatText    { unRepeatText :: Text }
newtype HelpText      = HelpText      { unHelpText   :: Text }

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
