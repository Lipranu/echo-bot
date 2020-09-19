{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Internal where

import Control.Concurrent.MVar ( MVar )
import Control.Monad.Reader    ( MonadReader, asks )
import Control.Monad.State     ( MonadState, gets )

type Lock = MVar ()

class Has a r where
  getter :: r -> a

obtain :: forall field env m . (Has field env, MonadReader env m) => m field
obtain = asks $ getter @field

grab :: forall field env m . (Has field env, MonadState env m) => m field
grab = gets $ getter @field
