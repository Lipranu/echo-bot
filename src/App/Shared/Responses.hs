{-# LANGUAGE OverloadedStrings #-}

module App.Shared.Responses
  ( Key
  , Response (..)
  ) where

import Infrastructure.Logger ( Loggable (..) )

import Control.Applicative  ( (<|>) )
import Control.Monad.Catch  ( Exception )
import Data.Aeson.Extended  ( FromJSON (..), Value (..), withObject, (.:) )

type Key = (Integer, Integer)

data Response e a
  = Success a
  | Error e

instance (Exception e, FromJSON e, FromJSON a)
  => FromJSON (Response e a) where
  parseJSON = withObject "App.Shared.Responses.Response" $ \o ->
        Error       <$> o .: "error"
    <|> Error       <$> parseJSON (Object o)
    <|> Success     <$> o .: "response"
    <|> Success     <$> o .: "result"
    <|> Success     <$> parseJSON (Object o)

instance (Loggable e, Loggable a) => Loggable (Response e a) where
  logData (Success x) = logData x
  logData (Error   x) = logData x
