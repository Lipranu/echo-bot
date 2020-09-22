{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Infrastructure.Has where

import Control.Monad.Reader    ( MonadReader, asks )
import Control.Monad.State     ( MonadState, gets )

class Has a r where
  getter :: r -> a

obtain :: forall field env m . (Has field env, MonadReader env m) => m field
obtain = asks $ getter @field

grab :: forall field env m . (Has field env, MonadState env m) => m field
grab = gets $ getter @field
