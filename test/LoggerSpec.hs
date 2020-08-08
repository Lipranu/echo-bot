{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts    #-}

module LoggerSpec (spec) where

import Logger hiding (State)

import Control.Monad.Reader
import Control.Monad.State
import Data.Text
import Data.Time
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

type Test = ReaderT Priority (State Text)

instance MonadTime Test where
  getTime = return time

instance MonadLog Test where
  logger lvl msg = showLog lvl msg <$> getTime >>= modify . (<>)

instance Arbitrary Priority where
  arbitrary = oneof $ pure <$> [Debug, Info, Warning, Error]

message :: Text
message = "example"

priority :: Priority
priority = Error

time :: UTCTime
time = UTCTime
  { utctDay = fromGregorian 0 1 1
  , utctDayTime = secondsToDiffTime 0
  }

showLogSpec :: Spec
showLogSpec = describe "showLog" $
  it "logging in proper format" $
    let result = showLog priority message time
        test   = showt time <> " - [Error]:\nexample\n"
     in result `shouldBe` test

logPureSpec :: Spec
logPureSpec = describe "logPure" $ do
  it "logs messages" $
    let result = execState (runReaderT (logPure priority message) priority) ""
        test   = showLog priority message time
     in result `shouldBe` test

  prop "logs only messages with equal or higher priority \
       \than specified in the configuration" $ \lvl config ->
    let result  = execState (runReaderT (logPure lvl message) config) ""
        test    | lvl >= config    = showLog lvl message time
                | otherwise = ""
     in result == test

spec :: Spec
spec = do
  showLogSpec
  logPureSpec
