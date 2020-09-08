{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module App.Vk.Converters ( Convertible (..) ) where

import App.Vk.Requests
import App.Vk.Responses

class Convertible a b | a -> b where
  convert :: a -> b
