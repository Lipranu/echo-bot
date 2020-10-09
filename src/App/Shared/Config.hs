{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Shared.Config
  ( Config (..)
  , DefaultRepeat (..)
  , HelpText (..)
  , RepeatText (..)
  , SharedReader
  ) where

-- IMPORTS -----------------------------------------------------------------

import Infrastructure.Has

import Control.Monad.Reader ( MonadReader )
import Data.Aeson           ( FromJSON (..), (.:), withObject )
import Data.Text            ( Text )

-- TYPES AND INSTANCES -----------------------------------------------------

type SharedReader r m = (MonadReader r m, Has HelpText r, Has RepeatText r)

newtype RepeatText    = RepeatText    { unRepeatText    :: Text }
newtype HelpText      = HelpText      { unHelpText      :: Text }
newtype DefaultRepeat = DefaultRepeat { unDefaultRepeat :: Int }

data Config = Config
  { cDefaultRepeat :: DefaultRepeat
  , cRepeatText    :: RepeatText
  , cHelpText      :: HelpText
  }

instance FromJSON Config where
  parseJSON = withObject path $ \o -> do
    cRepeatText    <- RepeatText <$> o .: "repeat_text"
    cHelpText      <- HelpText   <$> o .: "help_text"
    cDefaultRepeat <- o .: "default_repeat" >>= \(i :: Int) ->
      if   i > 0 && i < 6
      then pure $ DefaultRepeat i
      else fail $ path
        <> ".DefaultRepeat: the value should be between 1 and 5, but got: "
        <> show i
    pure Config {..}
    where path = "App.Shared.Config"
