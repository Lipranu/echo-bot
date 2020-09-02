module Data.Text.Extended
  ( module Data.Text
  , showt
  ) where

import Data.Text

showt :: Show a => a -> Text
showt = pack . show
