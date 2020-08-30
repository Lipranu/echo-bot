{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE FlexibleContexts            #-}

module Internal where

import Control.Concurrent.MVar        ( MVar )

class Has a r where
  getter :: r -> a

instance Has a a where
  getter = id

type Lock = MVar ()
