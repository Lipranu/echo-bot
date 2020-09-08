{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}

module App.Vk.Converters ( Convertible (..) ) where

-- IMPORTS -----------------------------------------------------------------

import App.Vk.Requests
import App.Vk.Responses

import Data.Bifunctor              ( bimap )
import Data.Maybe                  ( fromMaybe )
import Data.Text.Encoding.Extended ( encodeUtf8 )

import qualified Data.Text.Extended as Text

-- CLASSES -----------------------------------------------------------------

class Convertible a b | b -> a where
  convert :: a -> b

-- TYPES AND INSTANCES -----------------------------------------------------

instance Convertible LongPollServer GetUpdates where
  convert LongPollServer {..} =
    let guKey            = encodeUtf8 lpsKey
        guTs             = encodeUtf8 lpsTs
        (guHost, guPath) = bimap encodeUtf8 encodeUtf8
                         $ Text.span (/='/')
                         $ fromMaybe lpsServer
                         $ Text.stripPrefix "https://" lpsServer
     in GetUpdates {..}
