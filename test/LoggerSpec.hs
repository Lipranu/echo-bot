{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module LoggerSpec where

import Logger
import Internal (Lock, Has(..))

import Data.Aeson (decode)
import Data.Text
--import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Bifunctor
import Control.Concurrent.MVar
import Data.Time
import Data.Maybe
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Control.Monad.Reader
import Control.Monad.State
import Prelude hiding (log)

type TestState = (Text, Map FilePath Text)
data TestEnv = TestEnv
  { logger :: Logger Test
  , lock   :: Lock
  }

instance Has (Logger Test) TestEnv where
  getter = logger

instance Has Lock TestEnv where
  getter = lock

newtype Test a = Test { unTest :: ReaderT TestEnv (StateT TestState IO) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader TestEnv
           , MonadState TestState
           , MonadIO
           )

instance MonadTime Test where
  getTime = pure $ UTCTime
    { utctDay = fromGregorian 0 1 1
    , utctDayTime = secondsToDiffTime 0
    }

instance MonadLogger Test where
  logConsole msg   = modify $ first $ (<>) msg
  logFile path msg = modify $ second $
    Map.insertWithKey (\_ old new -> old <> new) path msg

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

instance Arbitrary Priority where
  arbitrary = elements [Debug, Info, Warning, Error]

instance Arbitrary Options where
  arbitrary = Options
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Config where
  arbitrary = Config
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

runTest :: Test a -> TestEnv -> IO (Text, Map FilePath Text)
runTest app logger = execStateT
  (runReaderT (unTest app) logger)
  (empty, Map.empty)

defaultOptions :: Options
defaultOptions = Options
  { oEnable   = True
  , oPriority = Warning
  , oShowTime = False
  , oShowMode = True
  }

defaultConfig :: Config
defaultConfig = Config
  { consoleOptions = defaultOptions
  , fileOptions    = defaultOptions
  , logFilePath    = "log"
  }

--runConsole :: Test a -> Logger Test -> Text
--runConsole app = fst . runTest app

--runFile :: Test a -> Logger Test -> Map FilePath Text
--runFile app = snd . runTest app

--consoleLoggerSpec :: Spec
--consoleLoggerSpec = describe "consoleLogger" $
--  prop "work" $ \msg lvl ->
--    let result = runConsole (log lvl msg) consoleLogger
--        test   = pack $ show $ Message lvl msg Nothing Nothing
--     in result `shouldBe` test
--
--fileLoggerSpec :: Spec
--fileLoggerSpec = describe "fileLogger" $ do
--  prop "work with correct path" $ \lvl msg ->
--    forAll (listOf1 arbitrary) $ \path ->
--      let result = (runFile (log lvl msg) $ fileLogger $ Just path) ! path
--          test   = pack $ show $ Message lvl msg Nothing Nothing
--       in result `shouldBe` test
--  prop "log is empty then filepath is empty string or Nothing" $ \lvl msg ->
--    forAll (elements [Nothing, Just ""]) $ \path ->
--      let result = runFile (log lvl msg) $ fileLogger path
--       in result `shouldSatisfy` Map.null
--  it "log is empty then filepath is abscent" $
--    let result = runFile (logInfo "test") $ fileLogger Nothing
--     in result `shouldSatisfy` Map.null
--
--enableLoggerSpec :: Spec
--enableLoggerSpec = describe "enablelogger" $ do
--  prop "log then true or Nothing" $ \lvl msg ->
--    forAll (listOf1 arbitrary) $ \path ->
--      forAll (elements [Just True, Nothing]) $ \b ->
--        let result a = runTest (log lvl msg) $ enableLogger b a
--            resultC  = fst $ result consoleLogger
--            resultF  = (snd $ result $ fileLogger $ Just path) ! path
--            test     = pack $ show $ Message lvl msg Nothing Nothing
--         in resultC <> resultF `shouldBe` test <> test
--  prop "not log then false" $ \lvl msg path ->
--    let result a = runTest (log lvl msg) $ enableLogger (Just False) a
--        resultC  = fst $ result consoleLogger
--        resultF  = snd $ result $ fileLogger path
--     in (resultC, Map.null resultF) `shouldBe` ("", True)
--
--filterLoggerSpec :: Spec
--filterLoggerSpec = describe "filterlogger" $
--  prop "log only equel an highter priority logs" $ \conf lvl msg ->
--    let result a = runTest (log lvl msg) $ filterLogger (Just conf) a
--        resultC  = fst $ result consoleLogger
--        resultF  = snd $ result $ fileLogger $ Just "path"
--        test     | lvl < conf = ""
--                 | otherwise  = pack $ show $ Message lvl "" Nothing Nothing
--     in resultC `shouldBe` test

decodePrioritySpec :: Spec
decodePrioritySpec = describe "decode priority json" $ do
  it "debug"   $ result "\"debug\""   `shouldBe` Just Debug
  it "info"    $ result "\"info\""    `shouldBe` Just Info
  it "warning" $ result "\"warning\"" `shouldBe` Just Warning
  it "error"   $ result "\"error\""   `shouldBe` Just Error
  it "invalid" $ result "{}"          `shouldBe` Nothing
  it "empty"   $ result ""            `shouldBe` Nothing
  where result t = decode t :: Maybe Priority

decodeOptionsSpec :: Spec
decodeOptionsSpec = describe "decode options json" $ do
  it "valid"   $ result valid   `shouldBe` Just (Options True Info True False)
  it "invalid" $ result invalid `shouldBe` Nothing
  it "empty"   $ result "{}"    `shouldBe` Just defaultOptions
  where result t = decode t :: Maybe Options
        invalid  = "{\"enable\":1}"
        valid    = "{\"enable\":true,\
                   \\"priority\":\"info\",\
                   \\"show_time\":true,\
                   \\"show_mode\":false}"

decodeConfigSpec :: Spec
decodeConfigSpec = describe "decode config json" $ do
  it "valid"   $ result validC  `shouldBe` Just testC
  it "invalid" $ result invalid `shouldBe` Nothing
  it "empty"   $ result "{}"    `shouldBe` Just defaultConfig
  where result t = decode t :: Maybe Config
        invalid  = "{\"log_path\":\"\"}"
        testO    = Options True Info True False
        testC    = Config testO testO "path"
        validC   = "{\"console_logger\":" <> validO
                <> ",\"file_logger\":"    <> validO
                <> ",\"log_path\":\"path\"}"
        validO   = "{\"enable\":true,\
                   \\"priority\":\"info\",\
                   \\"show_time\":true,\
                   \\"show_mode\":false}"
--prop_same :: Options -> Text -> Priority -> Text -> Property
prop_same options mode lvl msg = do
  lock <- newMVar ()
  let config = Config options options "path"
  result <- runTest (log lvl msg) $ TestEnv (mkLogger config mode) lock
  fst result `shouldBe` fromMaybe "" (snd result !? "path")

mkLoggerSpec :: Spec
mkLoggerSpec = describe "mkConfig" $ do
  modifyMaxSuccess (const 1000) $
    prop "if filepath is valid then with" prop_same
  it "f" $ 1 + 1 `shouldBe` 2
--    $ \options lvl msg -> do
--    lock <- newMVar ()
--    let config = Config options options $ Just "path"
--    result <- runTest (log lvl msg) $ TestEnv (mkLogger config "Test") lock
--    fst result `shouldBe` fromMaybe "" ((snd result) !? "path")

spec :: Spec
spec = do
--  consoleLoggerSpec
--  fileLoggerSpec
--  enableLoggerSpec
--  filterLoggerSpec
  decodePrioritySpec
  decodeOptionsSpec
  decodeConfigSpec
  mkLoggerSpec
