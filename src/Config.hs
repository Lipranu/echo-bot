{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia   #-}

module Config ( Config (..) ) where

import qualified App.Shared.Config     as S
import qualified App.Telegram.Config   as T
import qualified App.Vk.Config         as V
import qualified Infrastructure.Logger as L

import Data.Aeson.Extended ( FromJSON, DropPrefix (..) )
import GHC.Generics        ( Generic )

data Config = Config
  { cLogger   :: L.Config
  , cShared   :: S.Config
  , cTelegram :: T.Config
  , cVk       :: V.Config
  } deriving stock Generic
    deriving FromJSON via DropPrefix Config
