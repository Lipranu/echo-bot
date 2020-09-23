{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Telegram.Responses
  ( Response (..)
  , Update (..)
  ) where

-- IMPORTS --------------------------------------------------------------------

import Infrastructure.Logger hiding ( Priority (..) )

import Control.Applicative ( (<|>) )
import Data.Aeson.Extended ( (.:) )
import Data.Text.Extended  ( Text )

import qualified Data.Aeson.Extended as Aeson
import qualified Data.Text.Extended  as Text

-- TYPES AND INSTANCES -----------------------------------------------------

-- Response ----------------------------------------------------------------

data Response a
  = Succes a
  | Error Integer Text

instance Aeson.FromJSON a => Aeson.FromJSON (Response a) where
  parseJSON = Aeson.withObject "App.Vk.Response" $ \o ->
        Succes <$> o .: "result"
    <|> Error  <$> o .: "error_code"
               <*> o .: "description"

instance Loggable a => Loggable (Response a) where
  toLog (Succes x) = toLog x

  toLog (Error code description)
    = "An error occurred as a result of the request\n\
    \ | Error Code: "        <> Text.showt code <> "\n\
    \ | Error Description: " <> description

-- Update ------------------------------------------------------------------

newtype Update = Post Integer

instance Aeson.FromJSON Update where
  parseJSON = Aeson.withObject "App.Vk.Update" $ \o -> Post
    <$> o .: "update_id"

instance Loggable [Update] where
  toLog v = "Updates resived: " <> Text.showt (length v)

instance Loggable Update where
  toLog (Post i) = "Proccess post with id: " <> Text.showt i
