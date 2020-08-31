{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Infrastructure.RequesterSpec ( spec ) where

-- IMPORTS ---------------------------------------------------------------------

import Internal
import Infrastructure.Requester

import Control.Monad.Reader         ( Reader, asks )
import Network.HTTP.Client.Extended ( HttpException, Response )
import Test.Hspec                   ( Spec, context, describe, it, shouldBe )
import Test.Hspec.QuickCheck        ( prop )
import Test.QuickCheck              ( Arbitrary (..), elements )

import qualified Data.ByteString.Lazy as BSL

-- TYPES AND INSTANCES ---------------------------------------------------------

type Test = Reader Env

data Env = Env
  { envRequester :: Requester Test
  , envResult    :: Either HttpException (Response BSL.ByteString)
  }

instance MonadRequester Test where
  requester _ _ = asks envResult

instance Has (Requester Test) Env where
  getter = envRequester

-- FUNCTIONS -------------------------------------------------------------------



-- TESTS -----------------------------------------------------------------------

spec :: Spec
spec = describe "init" $ it "test" $
  1 + 1 `shouldBe` 2
