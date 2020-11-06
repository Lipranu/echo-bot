{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Infrastructure.Logger
  ( Config (..)
  , HasLogger
  , Lock
  , LogDebug (..)
  , LogError (..)
  , Loggable (..)
  , Logger (..)
  , MonadLogger (..)
  , MonadTime (..)
  , Options (..)
  , Priority (..)
  , log
  , logDebug
  , logError
  , logInfo
  , logWarning
  , mkLogEntry
  , mkLogEntryLine
  , mkLogger
  ) where

-- IMPORTS -----------------------------------------------------------------

import Infrastructure.Has

import Control.Concurrent.MVar ( MVar, takeMVar, putMVar )
import Control.Monad           ( when )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Control.Monad.Reader    ( MonadReader, lift )
import Control.Monad.State     ( StateT (..) )
import Data.Aeson              ( (.:?), (.!=) )
import Data.Bool               ( bool )
import Data.Char               ( isLower, isUpper, toUpper )
import Data.Proxy              ( Proxy (..) )
import Data.Text.Extended      ( Text )
import Data.Time               ( UTCTime )
import Data.Typeable           ( Typeable, typeOf )
import GHC.Generics            ( Generic, (:*:) (..), (:+:) (..), )
import GHC.TypeLits            ( Nat, KnownNat, KnownSymbol, type (+)
                               , symbolVal, natVal )
import Network.HTTP.Client     ( HttpException )

import qualified Data.Aeson         as Aeson
import qualified Data.Text.Extended as Text
import qualified GHC.Generics       as G

import Prelude hiding ( log )

-- CLASSES -----------------------------------------------------------------

class Monad m => MonadTime m where
  getTime :: m UTCTime

class Monad m => MonadLogger m where
  logConsole :: Text -> m ()
  logFile    :: FilePath -> Text -> m ()

class Loggable a where
  logData :: HasLogger r m => a -> m ()

data Context
  = Entry
  | Single
  | Product

data DataNameState
  = SkipDataName
  | AddDataName

class GLog (k :: Context) (r :: DataNameState) (i :: Nat) f where
  gLog :: f a -> Text

class GLogList (i :: Nat) f where
  gLogList :: Int -> f a -> Text

---------------------------------------------------------------
-- Entry
---------------------------------------------------------------

instance (GLog Entry r i f, G.Constructor c)
  => GLog Entry r i (G.C1 c f) where
    gLog f@(G.M1 x) = toCapitalCase (G.conName f) <> gLog @Entry @r @i x

instance GLog Entry r i G.U1 where
    gLog _ = ""

instance GLog Single SkipDataName i f => GLog Entry r i (G.S1 s f) where
    gLog (G.M1 x) = ": " <> nothingIfEmpty (gLog @Single @SkipDataName @i x)

instance (GLog Product r (i + 1) f, GLog Product r (i + 1) g) =>
  GLog Entry r i (f :*: g) where
    gLog (x :*: y) = ": " <> nothingIfEmpty result
      where result = gLog @Product @r @(i + 1) x
                  <> gLog @Product @r @(i + 1) y

---------------------------------------------------
-- Single
---------------------------------------------------

instance (G.Constructor c) =>
  GLog Single r i (G.C1 c G.U1) where
  gLog f@(G.M1 x) = toCapitalCase $ G.conName f

instance GLog Single SkipDataName i f =>
  GLog Single r i (G.C1 c (G.S1 s f)) where
  gLog (G.M1 (G.M1 x)) = gLog @Single @SkipDataName @i x

instance (GLog Product SkipDataName (i + 1) f, GLog Product SkipDataName (i + 1) g) =>
  GLog Single r i (G.C1 c (f :*: g)) where
  gLog (G.M1 (x :*: y)) = gLog @Product @SkipDataName @(i + 1) x
    <> gLog @Product @SkipDataName @(i + 1) y

---------------------------------------------------
-- Product
---------------------------------------------------

instance ( GLog Product r (i + 1) f
         , GLog Product r (i + 1) g
         , G.Constructor c
         ) => GLog Product r i (G.C1 c (f :*: g)) where
  gLog con@(G.M1 (x :*: y)) = Text.concat
    [ toCapitalCase $ G.conName con
    , ": "
    , nothingIfEmpty $ resultF <> resultG
    ]
    where resultF = gLog @Product @r @(i + 1) x
          resultG = gLog @Product @r @(i + 1) y

instance ( GLog Product r i f
         , GLog Product r i g
         ) => GLog Product r i (f :*: g) where
  gLog (x :*: y) = Text.concat
    [ gLog @Product @r @i x
    , gLog @Product @r @i y
    ]

instance (GLog Single r i f, G.Constructor c) =>
  GLog Product r i (G.C1 c (G.S1 s f)) where
  gLog con@(G.M1 (G.M1 x)) = Text.concat
    [ toCapitalCase $ G.conName con
    , ": "
    , nothingIfEmpty $ gLog @Single @r @i x
    ]

instance G.Constructor c => GLog Product r i (G.C1 c G.U1) where
  gLog = Text.pack . G.conName

instance (GLog Product SkipDataName i f, KnownSymbol n, KnownNat i) =>
  GLog Product r i (G.S1 (G.MetaSel (Just n) a b c) f) where
  gLog (G.M1 x) = bool line "" $ Text.null result
    where result = gLog @Product @SkipDataName @i x
          line   = Text.concat
            [ "\n  |"
            , Text.replicate (fromInteger $ natVal $ Proxy @i) "  "
            , toCapitalCase $ symbolVal $ Proxy @n
            , ": "
            , result
            ]

instance GLog Product AddDataName i f =>
  GLog Product r i (G.S1 (G.MetaSel Nothing a b c) f) where
  gLog (G.M1 x) = gLog @Product @AddDataName @i x

---------------------------------------------------
-- Common
---------------------------------------------------

instance GLog k r i f => GLog k r i (G.D1 d f) where
  gLog (G.M1 x) = gLog @k @r @i x

instance (GLog k r i f, GLog k r i g) => GLog k r i (f :+: g) where
  gLog (G.L1 x) = gLog @k @r @i x
  gLog (G.R1 x) = gLog @k @r @i x

instance (GLog k SkipDataName i (G.Rec0 a), Typeable a, KnownNat i) =>
  GLog k AddDataName i (G.Rec0 a) where
  gLog f@(G.K1 x) = bool line "" $ Text.null result
    where result = gLog @k @SkipDataName @i f
          line   = Text.concat
            [ "\n  |"
            , Text.replicate (fromInteger $ natVal $ Proxy @i) "  "
            ,Text.showt $ typeOf x
            , ": "
            , result
            ]

instance {-# OVERLAPPABLE #-}
  (Generic a, GLog k SkipDataName i (G.Rep a)) =>
  GLog k SkipDataName i (G.Rec0 a) where
  gLog (G.K1 x) = gLog @k @SkipDataName @i $ G.from x

instance GLog k SkipDataName i (G.Rec0 Int) where
  gLog (G.K1 x) = Text.showt x

instance GLog k SkipDataName i (G.Rec0 Integer) where
  gLog (G.K1 x) = Text.showt x

instance GLog k SkipDataName i (G.Rec0 Double) where
  gLog (G.K1 x) = Text.showt x

instance GLog k SkipDataName i (G.Rec0 Text) where
  gLog (G.K1 x) = x

instance GLog k SkipDataName i (G.Rec0 Aeson.Value) where
  gLog (G.K1 x) = Text.showt $ typeOf x

instance GLog k SkipDataName i (G.Rec0 a) =>
         GLog k SkipDataName i (G.Rec0 (Maybe a)) where
  gLog (G.K1 (Just x)) = gLog @k @SkipDataName @i @(G.Rec0 a) $ G.K1 x
  gLog (G.K1 Nothing)  = ""

instance (GLogList (i + 1) (G.Rep [a]), Typeable a) =>
  GLog k SkipDataName i (G.Rec0 [a]) where
  gLog (G.K1 x) = Text.concat
    [ Text.showt $ typeOf x
    , " (length: "
    , Text.showt $ length x
    , "): "
    , gLogList @(i + 1) 0 $ G.from x
    ]

---------------------------------------------------
-- List
---------------------------------------------------

instance GLogList i f => GLogList i (G.M1 a b f) where
  gLogList index (G.M1 x) = gLogList @i index x

instance GLogList i g => GLogList i (f :+: g) where
  gLogList _ (G.L1 _) = ""
  gLogList index (G.R1 x) = gLogList @i index x

instance ( GLog Entry SkipDataName i f
         , GLogList i g
         , KnownNat i
         ) => GLogList i ((G.S1 s f) :*: g) where
  gLogList index ((G.M1 x) :*: xs) = Text.concat
    [ "\n  |"
    , Text.replicate (fromInteger $ natVal $ Proxy @i) "  "
    , "Index "
    , Text.showt index
    , ": "
    , nothingIfEmpty $ gLog @Entry @SkipDataName @i x
    , gLogList @i index xs
    ]

instance (Generic a, GLogList i (G.Rep a)) => GLogList i (G.Rec0 a) where
  gLogList index (G.K1 x) = gLogList @i (index + 1) $ G.from x

-- TYPES AND INSTANCES -----------------------------------------------------

type Lock = MVar ()

type HasLogger r m =
  ( Has (Logger m) r
  , MonadReader r m
  , MonadTime m
  , MonadLogger m
  )

instance Loggable HttpException where
  logData e = logError $ mkLogEntry
    "HttpException:"
    [("Content", Text.showt e)]

newtype LogError a = LogError a

instance (Generic a, GLog Entry SkipDataName 0 (G.Rep a))
  => Loggable (LogError a) where
  logData (LogError x) = logError $ gLog @Entry @SkipDataName @0 $ G.from x

newtype LogDebug a = LogDebug a

instance (Generic a, GLog Entry SkipDataName 0 (G.Rep a))
  => Loggable (LogDebug a) where
  logData (LogDebug x) = logDebug $ gLog @Entry @SkipDataName @0 $ G.from x

data Priority
  = Debug
  | Info
  | Warning
  | Error
  deriving stock (Eq, Ord)

instance Aeson.FromJSON Priority where
  parseJSON = Aeson.withText path $ \case
    "debug"   -> pure Debug
    "info"    -> pure Info
    "warning" -> pure Warning
    "error"   -> pure Error
    e         -> fail $ path <> ": unknown priority: " <> Text.unpack e
    where path = "Infrastructure.Logger.Priority"

data Options = Options
  { oEnable   :: Bool
  , oPriority :: Priority
  , oShowTime :: Bool
  , oShowMode :: Bool
  }

instance Aeson.FromJSON Options where
  parseJSON = Aeson.withObject "Infrastructure.Logger.Options"
    $ \o -> Options
    <$> o .:? "enable"    .!= True
    <*> o .:? "priority"  .!= Warning
    <*> o .:? "show_time" .!= False
    <*> o .:? "show_mode" .!= True

data Config = Config
  { consoleOptions :: Options
  , fileOptions    :: Options
  , logFilePath    :: FilePath
  }

instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "Infrastructure.Logger.Config" $ \o -> do
    cl   <- o .:? "console_logger" .!= defaultOptions
    fl   <- o .:? "file_logger"    .!= defaultOptions
    path <- o .:? "log_path"       .!= "log"
    case path of
      ""   -> fail "Infrastructure.Logger.Config: empty log path"
      path -> return $ Config cl fl path
    where defaultOptions = Options
            { oEnable   = True
            , oPriority = Warning
            , oShowTime = False
            , oShowMode = True
            }

data Message = Message
  { mPriority :: Priority
  , mText     :: Text
  , mTime     :: Maybe UTCTime
  , mMode     :: Maybe Text
  }

newtype Logger m = Logger { runLogger :: Message -> m () }

instance Applicative m => Semigroup (Logger m) where
  Logger a1 <> Logger a2 = Logger $ \m -> a1 m *> a2 m

instance Applicative m => Monoid (Logger m) where
  mempty = Logger $ \_ -> pure ()

instance (HasLogger r m) => Has (Logger (StateT s m)) r where
  getter env = Logger $ \message ->  StateT $ \s ->
    (,s) <$> runLogger (getter env) message

instance MonadLogger m => MonadLogger (StateT s m) where
  logConsole   = lift . logConsole
  logFile path = lift . logFile path

instance MonadTime m => MonadTime (StateT s m) where
  getTime = lift getTime

-- FUNCTIONS ---------------------------------------------------------------

log :: HasLogger r m => Priority -> Text -> m ()
log lvl msg = do
  logger <- obtain
  runLogger logger message
  where message = Message
          { mPriority = lvl
          , mText     = msg
          , mMode     = Nothing
          , mTime     = Nothing
          }

logDebug, logInfo, logWarning, logError :: HasLogger r m => Text -> m ()
logDebug   = log Debug
logInfo    = log Info
logWarning = log Warning
logError   = log Error

timeLogger :: MonadTime m => Bool -> Logger m -> Logger m
timeLogger True  logger = Logger $ \m -> do
  time <- getTime
  runLogger logger $ m { mTime = Just time }
timeLogger False logger = logger

modeLogger :: Bool -> Text -> Logger m -> Logger m
modeLogger True  t logger = Logger $ \m ->
  runLogger logger m { mMode = Just t }
modeLogger False _ logger = logger

filterLogger :: Monad m => Priority -> Logger m -> Logger m
filterLogger p logger = Logger $ \m ->
  when (mPriority m >= p) $ runLogger logger m

enableLogger :: Applicative m => Bool -> Logger m -> Logger m
enableLogger True  logger = logger
enableLogger False _      = mempty

consoleLogger :: MonadLogger m => Logger m
consoleLogger = Logger $ logConsole . messageToLogEntry

fileLogger :: MonadLogger m => FilePath -> Logger m
fileLogger path = Logger $ logFile path . messageToLogEntry

concurrentLogger :: (Has Lock r, MonadReader r m, MonadLogger m, MonadIO m)
                 => Logger m
                 -> Logger m
concurrentLogger logger = Logger $ \m -> do
  lock <- obtain
  _ <- liftIO $ takeMVar lock
  runLogger logger m
  liftIO $ putMVar lock ()

mkLogger :: (Has Lock r, HasLogger r m, MonadIO m)
         => Config
         -> Text
         -> Logger m
mkLogger Config {..} mode
   = builder consoleOptions consoleLogger
  <> builder fileOptions (fileLogger logFilePath)
  where builder Options {..} = enableLogger oEnable
                             . filterLogger oPriority
                             . modeLogger   oShowMode mode
                             . timeLogger   oShowTime
                             . concurrentLogger

messageToLogEntry :: Message -> Text
messageToLogEntry Message {..} = Text.concat [priority, mode, time, message]
  where
    priority = Text.concat ["[", pToText, "]"]

    pToText  = case mPriority of
      Debug   -> "Debug"
      Info    -> "Info"
      Warning -> "Warning"
      Error   -> "Error"

    message  = Text.concat [": ", mText, "\n"]

    mode     = case mMode of
      Nothing -> ""
      Just "" -> ""
      Just m  -> Text.concat [" {", m, "}"]

    time     = case mTime of
      Nothing -> ""
      Just t  -> Text.concat [" (", Text.showt t, ")"]

mkLogEntry :: Text -> [(Text, Text)] -> Text
mkLogEntry text lines = text
  <> Text.concat (mkLogEntryLine <$> lines)

mkLogEntryLine :: (Text, Text) -> Text
mkLogEntryLine (key, value) = "\n  |  " <> key <> ": " <> value

toCapitalCase :: String -> Text
toCapitalCase s = Text.pack $ dropPrefix s
  where dropPrefix (x:xs)
          | all isLower s = toUpper x : xs
          | otherwise     = (toUpper l :) $ foldr go "" ls
        go x xs
          | isUpper x = ' ' : x : xs
          | otherwise = x : xs
        (l:ls) = dropWhile isLower s

nothingIfEmpty :: Text -> Text
nothingIfEmpty t = bool t "Nothing" $ Text.null t
