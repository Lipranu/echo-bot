{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Shared.Routes
  ( MonadEffects
  --, handleErrorRequest
  --, handleWarningRequest
  --, handleWarningR
  --, traverseHandle
  --, withLog
  --, endRoute
  --, start
  ) where

-- IMPORTS -----------------------------------------------------------------

import Internal
import Infrastructure.Logger
import Infrastructure.Requester

import App.Vk.Responses (Response (..))

import Data.Text.Extended   ( Text )
import Data.Foldable        ( traverse_ )
import Control.Monad.Reader ( MonadReader )
import Data.Aeson.Extended  ( FromJSON )

-- TYPES -------------------------------------------------------------------

type MonadEffects r m =
  ( Has (Requester m) r
  , MonadRequester m
  , MonadReader r m
  , HasLogger r m
  )

-- FUNCTIONS ---------------------------------------------------------------

--endRoute :: Monad m => a -> m ()
--endRoute = const (return ())
--
--handle :: (HasLogger r m, HasPriority a)
--       => (Result (Response a) -> m ())  -- logger for input
--       -> m ()                           -- additional logger
--       -> (a -> m ())                    -- function for handled input
--       -> Result (Response a)            -- input
--       -> m ()                           -- phantom result
--handle _ _ route (Result (Success x)) = logData x >> route x
--handle logger1 logger2 _ error = logger1 error >> logger2
--
--handleR :: (HasLogger r m, HasPriority a)
--       => (Result a -> m ())
--       -> m ()
--       -> (a -> m ())
--       -> Result a
--       -> m ()
--handleR _ _ route (Result x) = logData x >> route x
--handleR logger1 logger2 _ error = logger1 error >> logger2
--
--handleWarningR :: (HasLogger r m, HasPriority a)
--       => (a -> m ())
--       -> Result a
--       -> m ()
--handleWarningR = handleR logWarning (return ())
--
--handleError, handleWarning
--  :: (HasLogger r m, HasPriority input)
--  => (input -> m ())
--  -> Result (Response input)
--  -> m ()
--handleError   = handle logError shutdown
--handleWarning = handle logWarning (return ())
--
--withLog :: (HasLogger r m, HasPriority a) => (a -> m b) -> a -> m b
--withLog f x = logData x >> f x
--
--requestWithLog :: ( FromJSON output
--                  , HasPriority input
--                  , MonadEffects r m
--                  , ToRequest m r input
--                  )
--               => input
--               -> m (Result (Response output))
--requestWithLog = withLog requestAndDecode
--
--handleErrorRequest, handleWarningRequest
--  :: forall output input r m
--   . ( FromJSON output
--     , HasPriority input
--     , HasPriority output
--     , MonadEffects r m
--     , ToRequest m r input
--     )
--  => input
--  -> (output -> m ())
--  -> m ()
--handleErrorRequest   x f = requestWithLog x >>= handleError f
--handleWarningRequest x f = requestWithLog x >>= handleWarning f
--
--traverseHandle :: (HasLogger r m, HasPriority input)
--               => (input -> m ())
--               -> [Result input]
--               -> m ()
--traverseHandle f = traverse_ (handleWarningR f)
--
--start, shutdown :: HasLogger r m => m ()
--start    = logInfo ("Application getting started" :: Text)
--shutdown = logInfo ("Application shut down" :: Text)
