{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleInstances    #-}

module LoggerSpec (spec) where

import Logger hiding (State)
import Test.Hspec
import Data.Text
import Control.Monad.Reader
import Control.Monad.State
import Data.Time

type Test = ReaderT Priority (State Text)

instance MonadTime Test where
  getTime = return exampleTime

instance MonadLog Test where
  logger lvl msg = do
    l <- showLog lvl msg
    modify (<> l)

exampleMessage :: Text
exampleMessage = "example"

examplePriority :: Priority
examplePriority = Error

exampleTime :: UTCTime
exampleTime = UTCTime
  { utctDay = fromGregorian 0 1 1
  , utctDayTime = secondsToDiffTime 0
  }

exampleShowLog :: Text
exampleShowLog = showt  exampleTime <> " - [Error]:\nexample\n"

showLogSpec :: Spec
showLogSpec = describe "showLog" $ do
  it "logging in proper format" $ do
    let test   = showLog examplePriority exampleMessage
        result = evalState (runReaderT test examplePriority) ("" :: Text)
    result `shouldBe` exampleShowLog

spec :: Spec
spec = do
  showLogSpec
