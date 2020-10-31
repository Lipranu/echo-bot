{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
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
  , GConst (..)
  , HasLogger
  , HasPriority (..)
  , Lock
  , Loggable (..)
  , Logger (..)
  , MonadLogger (..)
  , MonadTime (..)
  , Options (..)
  , Priority (..)
  , gMkLogEntry
  , log
  , logDebug
  , logError
  , logInfo
  , logWarning
  , mkLogLine
  , mkLogger
  , mkToLog
  ) where

-- IMPORTS -----------------------------------------------------------------

import Infrastructure.Has

import Control.Concurrent.MVar   ( MVar, takeMVar, putMVar )
import Control.Monad             ( when )
import Control.Monad.IO.Class    ( MonadIO, liftIO )
import Control.Monad.Reader      ( MonadReader, lift )
import Control.Monad.State       ( StateT (..) )
import Data.Aeson                ( (.:?), (.!=) )
import Data.Bool                 ( bool )
import Data.Char                 ( isLower, isUpper, toUpper )
import Data.Text.Encoding        ( decodeUtf8 )
import Data.Text.Extended        ( Text )
import Data.Time                 ( UTCTime )
import GHC.Generics              ( Generic, (:*:) (..), (:+:) (..), )
import Network.HTTP.Client       ( HttpException (..)
                                 , HttpExceptionContent (..)
                                 , responseStatus
                                 )
import Network.HTTP.Types.Status ( Status (..) )

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
  toLog :: a -> Text

class Loggable a => HasPriority a where
  logData :: HasLogger r m => a -> m ()

  default logData :: (Generic a, GLog (G.Rep a), HasLogger r m) => a -> m ()
  logData = logDebug . gMkLogEntry

class GLog f where
  glog :: Int -> f x -> Text

-- TYPES AND INSTANCES -----------------------------------------------------

type Lock = MVar ()

type HasLogger r m =
  ( Has (Logger m) r
  , MonadReader r m
  , MonadTime m
  , MonadLogger m
  )

instance GLog f => GLog (G.D1 c f) where
  glog i (G.M1 x) = glog i x

instance (GLog a, GLog b) => GLog (a :+: b) where
  glog i (G.L1 x) = glog i x
  glog i (G.R1 x) = glog i x

instance (GLog f, G.Constructor c) => GLog (G.C1 c f) where
  glog i c@(G.M1 x) = Text.pack (G.conName c)
    <> (bool "" ":" $ G.conIsRecord c)
    <> glog i x

instance (GLog a, GLog b) => GLog (a :*: b) where
  glog i (x :*: y) = glog i x <> glog i y

instance (GLog f, G.Selector c) => GLog (G.S1 c f) where
  glog i c@(G.M1 x)
    | Text.null field = ""
    | otherwise       = "\n | "
                     <> (Text.replicate i "\t")
                     <> Text.pack (mkSelector $ G.selName c)
                     <> field
    where field = glog i x

instance {-# OVERLAPPABLE #-}
  (Generic f, GLog (G.Rep f)) => GLog (G.Rec0 f) where
  glog i (G.K1 x) = gMkLogEntry' (i + 1) x

instance GLog (G.Rec0 a) => GLog (G.Rec0 (Maybe a)) where
  glog i (G.K1 x) = maybe "" (glog @(G.Rec0 a) i . G.K1) x

instance GLog (G.Rec0 [a]) where
  glog i (G.K1 xs) = Text.showt (length xs) <> " objects"

instance GLog (G.Rec0 Integer) where
  glog _ (G.K1 x) = Text.showt x

instance GLog (G.Rec0 Int) where
  glog _ (G.K1 x) = Text.showt x

instance GLog (G.Rec0 Double) where
  glog _ (G.K1 x) = Text.showt x

instance GLog (G.Rec0 Text) where
  glog _ (G.K1 x) = x

instance GLog (G.Rec0 String) where
  glog _ (G.K1 x) = Text.pack x

instance GLog G.U1 where glog _ _ = ""

instance Loggable Text   where toLog = id
instance Loggable String where toLog = Text.pack

instance Loggable HttpException where
  toLog (HttpExceptionRequest _ content) = toLog content

  toLog (InvalidUrlException url reason) = mkToLog "InvalidUrlException:"
    [ ("Description", "A URL is invalid for a given reason")
    , ("URL"        , Text.pack url)
    , ("Reason"     , Text.pack reason)
    ] []

instance HasPriority HttpException where logData = logError . toLog

instance Loggable HttpExceptionContent where
  toLog (StatusCodeException response _) = mkToLog "StatusCodeException:"
    [ ("Description", descr)
    , ("Code"       , code)
    , ("Message"    , message)
    ] []
    where code    = Text.showt $ statusCode    $ responseStatus response
          message = decodeUtf8 $ statusMessage $ responseStatus response
          descr   = "Generated by the parseUrlThrow function when the\
                    \ server returns a non-2XX response status code"

  toLog (TooManyRedirects _) = mkToLog "TooManyRedirects:"
    [("Description", descr)] []
    where descr = "The server responded with too\
                  \ many redirects for a request"

  toLog OverlongHeaders = mkToLog "OverlongHeaders:"
    [("Description", descr)] []
    where descr = "Either too many headers, or too many\
                  \ total bytes in a single header,\
                  \ were returned by the server"

  toLog ResponseTimeout = mkToLog "ResponseTimeout:"
    [("Description", "The server took too long to return a response")] []

  toLog ConnectionTimeout = mkToLog "ConnectionTimeout:"
    [("Description", "Attempting to connect to the server timed out")] []

  toLog (ConnectionFailure e) = mkToLog "ConnectionFailure:"
    [ ("Description", descr)
    , ("Exception"  , Text.showt e)
    ] []
    where descr = "An exception occurred when trying\
                  \ to connect to the server"

  toLog (InvalidStatusLine bs) = mkToLog "InvalidStatusLine:"
    [ ("Description", descr)
    , ("Content"    , decodeUtf8 bs)
    ] []
    where descr = "The status line returned by the server\
                  \ could not be parsed"

  toLog (InvalidHeader bs) = mkToLog "InvalidHeader:"
    [ ("Description", "The given response header line could not be parsed")
    , ("Content"    , decodeUtf8 bs)
    ] []

  toLog (InvalidRequestHeader bs) = mkToLog "InvalidRequestHeader:"
    [ ("Description", "The given request header is not compliant")
    , ("Content"    , decodeUtf8 bs)
    ] []

  toLog (InternalException e) = mkToLog "InternalException:"
    [ ("Description", descr)
    , ("Content"    , Text.showt e)
    ] []
    where descr = "An exception was raised by an underlying\
                  \ library when performing the request"

  toLog (ProxyConnectException host port status)
    = mkToLog "ProxyConnectException:"
    [ ("Description", descr)
    , ("Code"       , Text.showt $ statusCode status)
    , ("Message"    , decodeUtf8 $ statusMessage status)
    , ("Port"       , Text.showt port)
    , ("Host"       , decodeUtf8 host)
    ] []
    where descr = "A non-200 status code was returned when trying to\
                  \ connect to the proxy server on the given host and port"

  toLog NoResponseDataReceived = mkToLog "NoResponseDataReceived:"
    [("Description", descr)] []
    where descr = "No response data was received from the server at all.\
                  \ This exception may deserve special handling within the\
                  \ library, since it may indicate that a pipelining has\
                  \ been used, and a connection thought to be open was in\
                  \ fact closed"

  toLog TlsNotSupported = mkToLog "TlsNotSupported:"
    [("Description", descr)] []
    where descr = "Exception thrown when using a Manager\
                  \ which does not have support for secure connections"

  toLog (WrongRequestBodyStreamSize expected actual)
    = mkToLog "WrongRequestBodyStreamSize:"
    [ ("Description", descr)
    , ("Expected"   , Text.showt expected)
    , ("Actual"     , Text.showt actual)
    ] []
    where descr = "The request body provided did not match the expected size"

  toLog (ResponseBodyTooShort expected actual)
    = mkToLog "ResponseBodyTooShort:"
    [ ("Description", "The returned response body is too short")
    , ("Expected"   , Text.showt expected)
    , ("Actual"     , Text.showt actual)
    ] []

  toLog InvalidChunkHeaders = mkToLog "InvalidChunkHeaders:"
    [("Description", "A chunked response body had invalid headers")] []

  toLog IncompleteHeaders = mkToLog "IncompleteHeaders:"
    [("Description",descr)] []
    where descr = "An incomplete set of response headers were returned"

  toLog (InvalidDestinationHost host) = mkToLog "InvalidDestinationHost:"
    [ ("Description", "The host we tried to connect to is invalid")
    , ("Host"       , decodeUtf8 host)
    ] []

  toLog (HttpZlibException e) = mkToLog "HttpZlibException:"
    [ ("Description", descr)
    , ("Content"    , Text.showt e)
    ] []
    where descr = "An exception was thrown when inflating a response body"

  toLog (InvalidProxyEnvironmentVariable name value)
    = mkToLog "InvalidProxyEnvironmentVariable:"
    [ ("Description", "Values in the proxy environment variable were invalid")
    , ("Variable"   , name)
    , ("Value"      , value)
    ] []

  toLog ConnectionClosed = mkToLog "ConnectionClosed:"
    [("Description", descr)] []
    where descr = "Attempted to use a Connection which was already closed"

  toLog (InvalidProxySettings content) = mkToLog "InvalidProxySettings:"
    [ ("Description", "Proxy settings are not valid")
    , ("Content"    , content)
    ] []

instance HasPriority HttpExceptionContent where logData = logError . toLog

newtype GConst a = GConst a

instance Generic (GConst a) where
  type Rep (GConst a) = G.Rec0 a

  from (GConst x) = G.K1 x
  to (G.K1 x) = GConst x

data Priority
  = Debug
  | Info
  | Warning
  | Error
  deriving (Show, Eq, Ord)

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
  } deriving (Show, Eq)

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
  } deriving (Show, Eq)

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

instance Loggable Message where
  toLog Message {..} = priority <> mode <> time <> message
    where
      priority = "[" <> Text.showt mPriority <> "]"
      message  = ": " <> mText <> "\n"
      mode     = case mMode of
        Nothing -> ""
        Just "" -> ""
        Just m  -> " {" <> m <> "}"
      time     = case mTime of
        Nothing -> ""
        Just t  -> " (" <> Text.showt t <> ")"

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

log :: (HasLogger r m, Loggable a) => Priority -> a -> m ()
log lvl msg = do
  logger <- obtain
  runLogger logger message
  where message = Message
          { mPriority = lvl
          , mText     = toLog msg
          , mMode     = Nothing
          , mTime     = Nothing
          }

logDebug, logInfo, logWarning, logError
  :: (HasLogger r m, Loggable a)
  => a
  -> m ()
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
consoleLogger = Logger $ logConsole . toLog

fileLogger :: MonadLogger m => FilePath -> Logger m
fileLogger path = Logger $ logFile path . toLog

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

mkToLog :: Text -> [(Text, Text)] -> [(Text, Maybe Text)] -> Text
mkToLog text lines maybeLines = text
  <> Text.concat (mkLogLine <$> lines)
  <> Text.concat (maybeLine <$> maybeLines)

mkLogLine :: (Text, Text) -> Text
mkLogLine (key, value) = "\n | " <> key <> ": " <> value

maybeLine :: (Text, Maybe Text) -> Text
maybeLine (_, Nothing) = ""
maybeLine (key, Just value)
  | Text.null value = ""
  | otherwise       = mkLogLine (key, value)

gMkLogEntry' :: (Generic a, GLog (G.Rep a)) => Int -> a -> Text
gMkLogEntry' i = glog i . G.from

gMkLogEntry :: (Generic a, GLog (G.Rep a)) => a -> Text
gMkLogEntry = gMkLogEntry' 0

mkSelector :: String -> String
mkSelector [] = "Field: "
mkSelector s@(x:xs)
  | all isLower s = toUpper x : xs <> ": "
  | otherwise     = (toUpper l :) $ foldr go ": " ls
  where (l:ls) = dropWhile isLower s
        go x xs | isUpper x = ' ' : x : xs
                | otherwise = x : xs
