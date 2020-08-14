{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

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
