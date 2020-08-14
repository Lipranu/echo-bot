module Logger
  ( Config
  , Logger
  , MonadLogger (..)
  , MonadTime (..)

  , logDebug
  , logError
  , logInfo
  , logWarning
  , mkLogger
  ) where

import           Logger.Internal
