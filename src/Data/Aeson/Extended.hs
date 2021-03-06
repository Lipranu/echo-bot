{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Extended
  ( module Data.Aeson
  , DropPrefix (..)
  ) where

import Data.Aeson
import Data.Bool    ( bool )
import Data.Char    ( isLower )
import GHC.Generics ( Generic, Rep )

newtype DropPrefix a = DropPrefix a

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (DropPrefix a) where
  parseJSON v = DropPrefix <$> parseJsonDrop v
    where parseJsonDrop = genericParseJSON defaultOptions
            { fieldLabelModifier     = dropPrefix
            , constructorTagModifier = dropPrefix
            }

instance (Generic a, GToJSON Zero (Rep a)) => ToJSON (DropPrefix a) where
  toJSON (DropPrefix v) = toJsonDrop v
    where toJsonDrop = genericToJSON defaultOptions
            { fieldLabelModifier = dropPrefix
            , omitNothingFields  = True
            }

dropPrefix :: String -> String
dropPrefix str = camelTo2 '_' . bool prefix str $ null prefix
  where prefix = dropWhile isLower str
