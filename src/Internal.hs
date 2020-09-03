{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE FlexibleContexts            #-}

module Internal where

import Control.Concurrent.MVar ( MVar )
import Control.Monad.Reader    ( MonadReader, asks )

class Has a r where
  getter :: r -> a

instance Has a a where
  getter = id

obtain :: (Has a r, MonadReader r m) => m a
obtain = asks getter

type Lock = MVar ()
