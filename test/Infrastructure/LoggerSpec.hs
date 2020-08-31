{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables            #-}

module Infrastructure.LoggerSpec ( spec ) where

-- IMPORTS ---------------------------------------------------------------------

import Internal
import Infrastructure.Logger

import Control.Concurrent.Async ( concurrently )
import Control.Concurrent.MVar  ( newMVar )
import Control.Monad.IO.Class   ( MonadIO, liftIO )
import Control.Monad.Reader     ( ReaderT, MonadReader, asks, runReaderT )
import Data.Aeson               ( eitherDecode )
import Data.Bifunctor           ( bimap )
import Data.IORef               ( IORef, modifyIORef, newIORef, readIORef )
import Data.Map                 ( Map, (!), (!?) )
import Data.Maybe               ( fromMaybe )
import Data.Text                ( Text )
import Test.Hspec               ( Spec, context, describe, it, shouldBe )
import Test.Hspec.QuickCheck    ( prop )
import Test.QuickCheck          ( Arbitrary (..), elements )

import qualified Data.Map  as Map
import qualified Data.Text as Text
import qualified Data.Time as Time

import Prelude hiding (log)

-- TYPES AND INSTANCES ---------------------------------------------------------

type FileLog    = Map FilePath Text
type ConsoleLog = Text

data Env = Env
  { logger     :: Logger App
  , lock       :: Lock
  , fileLog    :: IORef FileLog
  , consoleLog :: IORef ConsoleLog
  }

instance Has (Logger App) Env where
  getter = logger

instance Has Lock Env where
  getter = lock

newtype App a = App { unTest :: ReaderT Env IO a } deriving
  (Functor, Applicative, Monad, MonadReader Env, MonadIO)

instance MonadTime App where
  getTime = pure testTime

instance MonadLogger App where
  logConsole msg = do
    log <- asks consoleLog
    liftIO $ modifyIORef log (<> msg)

  logFile path msg = do
    log <- asks fileLog
    liftIO $ modifyIORef log
           $ Map.insertWithKey (\_ old new -> old <> new) path msg

instance Arbitrary Text where
  arbitrary = Text.pack <$> arbitrary

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

-- FUNCTIONS -------------------------------------------------------------------

mkEnv :: Logger App -> IO Env
mkEnv logger = Env logger
  <$> newMVar ()
  <*> newIORef mempty
  <*> newIORef mempty

runTest :: App a -> Env -> IO a
runTest app = runReaderT (unTest app)

getResult :: Env -> IO (ConsoleLog, FileLog)
getResult Env {..} = (,)
  <$> readIORef consoleLog
  <*> readIORef fileLog

testTime :: Time.UTCTime
testTime = Time.UTCTime
  { utctDay     = Time.fromGregorian 0 1 1
  , utctDayTime = Time.secondsToDiffTime 0
  }

defaultOptions, testOptions :: Options
defaultOptions = Options
  { oEnable   = True
  , oPriority = Warning
  , oShowTime = False
  , oShowMode = True
  }
testOptions = Options
  { oEnable   = True
  , oPriority = Debug
  , oShowTime = True
  , oShowMode = True
  }

defaultConfig, testConfig :: Config
defaultConfig = Config
  { consoleOptions = defaultOptions
  , fileOptions    = defaultOptions
  , logFilePath    = "log"
  }
testConfig = Config
  { consoleOptions = testOptions
  , fileOptions    = testOptions
  , logFilePath    = "log"
  }

-- TESTS -----------------------------------------------------------------------

decodePrioritySpec :: Spec
decodePrioritySpec = describe "decode priority json" $ do
  it "debug"      $ result "\"debug\""   `shouldBe` Right Debug
  it "info"       $ result "\"info\""    `shouldBe` Right Info
  it "warning"    $ result "\"warning\"" `shouldBe` Right Warning
  it "error"      $ result "\"error\""   `shouldBe` Right Error
  it "invalid"    $ result "\"e\""       `shouldBe` Left invalid
  it "wrong type" $ result "{}"          `shouldBe` Left wrongType
  where
    result t  = eitherDecode t :: Either String Priority

    invalid   = "Error in $: Logger.Priority: unknown priority: e"

    wrongType = "Error in $: parsing Logger.Priority failed,\
                \ expected String, but encountered Object"


decodeOptionsSpec :: Spec
decodeOptionsSpec = describe "decode options json" $ do
  it "valid"        $ result valid  `shouldBe` Right testOptions
  it "wrong type"   $ result "\"\"" `shouldBe` Left wrongType
  it "empty object" $ result "{}"   `shouldBe` Right defaultOptions
  where
    result t  = eitherDecode t :: Either String Options

    wrongType = "Error in $: parsing Logger.Options failed,\
                \ expected Object, but encountered String"

    valid     = "{\"enable\":true,\"priority\":\"debug\",\
                \\"show_time\":true,\"show_mode\":true}"

decodeConfigSpec :: Spec
decodeConfigSpec = describe "decode config json" $ do
  it "valid"        $ result valid   `shouldBe` Right testConfig
  it "wrong type"   $ result "\"\""  `shouldBe` Left wrongType
  it "empty path"   $ result invalid `shouldBe` Left emptyPath
  it "empty object" $ result "{}"    `shouldBe` Right defaultConfig
  where
    result t  = eitherDecode t :: Either String Config

    wrongType = "Error in $: parsing Logger.Config failed,\
                \ expected Object, but encountered String"

    emptyPath = "Error in $: Logger.Config: empty log path"

    invalid   = "{\"log_path\":\"\"}"

    valid     = "{\"console_logger\":"
             <> options
             <> ",\"file_logger\":"
             <> options
             <> ",\"log_path\":\"log\"}"

    options   = "{\"enable\":true,\"priority\":\"debug\",\
                \\"show_time\":true,\"show_mode\":true}"

logSpec :: Spec
logSpec = describe "log" $ do
  context "console" $ do
    it "logDebug"   $ rConsole logDebug   >>= (`shouldBe` test "Debug")
    it "logInfo"    $ rConsole logInfo    >>= (`shouldBe` test "Info")
    it "logWarning" $ rConsole logWarning >>= (`shouldBe` test "Warning")
    it "logError"   $ rConsole logError   >>= (`shouldBe` test "Error")

  context "file" $ do
    it "logDebug"   $ rFile    logDebug   >>= (`shouldBe` test "Debug")
    it "logInfo"    $ rFile    logInfo    >>= (`shouldBe` test "Info")
    it "logWarning" $ rFile    logWarning >>= (`shouldBe` test "Warning")
    it "logError"   $ rFile    logError   >>= (`shouldBe` test "Error")
  where
    result action = do
      env <- mkEnv $ mkLogger testConfig "Test"
      runTest (action ("test" :: Text)) env
      return $ getResult env

    rConsole action = result action >>= fmap fst

    rFile    action = result action >>= fmap ((! "log") . snd)

    test        lvl = "[" <> lvl <> "] {Test} ("
                   <> (Text.pack . show) testTime <> "): test\n"

loggerMonoidLawSpec :: Spec
loggerMonoidLawSpec = describe "Logger monoid laws" $ do
  prop "left identity" $ \config mode lvl (msg :: Text) -> do
    env1         <- mkEnv $ mkLogger config mode
    env2         <- mkEnv $ mempty <> mkLogger config mode
    (res1, res2) <- runBoth env1 env2 $ log lvl msg
    res1 `shouldBe` res2

  prop "right identity" $ \config mode lvl (msg :: Text) -> do
    env1         <- mkEnv $ mkLogger config mode
    env2         <- mkEnv $ mkLogger config mode <> mempty
    (res1, res2) <- runBoth env1 env2 $ log lvl msg
    res1 `shouldBe` res2

  prop "associativity" $ \c1 c2 c3 m1 m2 m3 lvl (msg :: Text) -> do
    let logger1 = mkLogger c1 m1
        logger2 = mkLogger c2 m2
        logger3 = mkLogger c3 m3
    env1         <- mkEnv $ (logger1 <> logger2) <> logger3
    env2         <- mkEnv $ logger1 <> (logger2 <> logger3)
    (res1, res2) <- runBoth env1 env2 $ log lvl msg
    res1 `shouldBe` res2
  where
    runBoth env1 env2 action = do
      runTest action env1
      runTest action env2
      res1 <- getResult env1
      res2 <- getResult env2
      return (res1, res2)

loggerPropertiesSpec :: Spec
loggerPropertiesSpec = describe "Logger properties" $ do
  prop "with the same settings, the same messages are logged \
       \to the console and file" $ \options lvl (msg :: Text) -> do
    env <- mkEnv $ mkLogger (Config options options "path") "Test"
    runTest (log lvl msg) env
    (console, file) <- getResult env
    console `shouldBe` fromMaybe "" (file !? "path")

  prop "does not log messages with lower priority \
       \than in the config" $ \clvl lvl (msg :: Text) -> do
    let options = testOptions { oPriority = clvl }
        config  = testConfig  { consoleOptions = options
                              , fileOptions    = options
                              }
    env <- mkEnv $ mkLogger config "Test"
    runTest (log lvl msg) env
    res <- getResult env
    bimap Text.null Map.null res `shouldBe` (lvl < clvl, lvl < clvl)

  prop "concurrent logging" $ \config lvl (msg :: Text) -> do
    env1 <- mkEnv $ mkLogger config "Test"
    env2 <- mkEnv $ mkLogger config "Test"
    concurrently (runTest (log lvl msg) env1) (runTest (log lvl msg) env1)
    runTest (log lvl msg >> log lvl msg) env2
    res1 <- getResult env1
    res2 <- getResult env2
    res1 `shouldBe` res2

spec :: Spec
spec = do
  decodePrioritySpec
  decodeOptionsSpec
  decodeConfigSpec
  logSpec
  loggerMonoidLawSpec
  loggerPropertiesSpec
