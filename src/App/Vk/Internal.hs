{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module App.Vk.Internal
  ( Token (..)
  , Group (..)
  , VkReader
  ) where

import Control.Monad.Reader        ( MonadReader )
import Data.Text.Extended          ( Text )
import Internal                    ( Has )

newtype Token = Token { unToken :: Text }

newtype Group = Group { unGroup :: Text }

type VkReader r m = (Has Token r, Has Group r, MonadReader r m)
