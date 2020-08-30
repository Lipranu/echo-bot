{-# LANGUAGE FlexibleContexts #-}

module Data.Aeson.Extended
  ( module Data.Aeson
  , parseJsonDrop
  , toJsonDrop
  ) where

import Data.Aeson
import Data.Aeson.Types ( Parser )
import Data.Char        ( isUpper, toLower, isLower )
import GHC.Generics     ( Generic, Rep )

parseJsonDrop :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
parseJsonDrop = genericParseJSON defaultOptions
  { fieldLabelModifier = fieldToJsonKey }

toJsonDrop :: (Generic a, GToJSON Zero (Rep a)) => a -> Value
toJsonDrop = genericToJSON defaultOptions
  { fieldLabelModifier = fieldToJsonKey
  , omitNothingFields = True
  }

fieldToJsonKey :: String -> String
fieldToJsonKey str
  | all isLower str = str
  | otherwise       = foldr f ""
                    $ (\(x:xs) -> toLower x : xs)
                    $ dropWhile isLower str
  where f x y | isUpper x = '_' : toLower x : y
              | otherwise = x : y
