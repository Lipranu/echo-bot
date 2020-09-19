{-# LANGUAGE OverloadedStrings          #-}

module App.Shared.Responses
  ( Key
  , Response (..)
  ) where

import Infrastructure.Logger ( Loggable (..), HasPriority (..) )

import Control.Monad.Catch  ( Exception )
import Data.Aeson.Extended  ( FromJSON (..), Value (..), withObject, (.:) )
import Control.Applicative  ( (<|>) )

type Key = (Integer, Integer)

data Response e a
  = Success a
  | Error e

instance (Exception e, FromJSON e, FromJSON a) => FromJSON (Response e a) where
  parseJSON = withObject "App.Vk.Response" $ \o ->
        Error       <$> o .: "error"
    <|> Error       <$> parseJSON (Object o)
    <|> Success     <$> o .: "response"
    <|> Success     <$> o .: "result"
    <|> Success     <$> parseJSON (Object o)

instance (Loggable e, Loggable a) => Loggable (Response e a) where
  toLog (Success x) = toLog x
  toLog (Error   x) = toLog x

instance (HasPriority e, HasPriority a) => HasPriority (Response e a) where
  logData (Success x) = logData x
  logData (Error   x) = logData x
