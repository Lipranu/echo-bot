module Data.Text.Encoding.Extended
  ( module Data.Text.Encoding
  , encodeShowUtf8
  ) where

import Data.Text.Encoding
import Data.Text          ( pack )
import Data.ByteString    ( ByteString )

encodeShowUtf8 :: Show a => a -> ByteString
encodeShowUtf8 = encodeUtf8 . pack . show
