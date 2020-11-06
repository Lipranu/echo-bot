{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DerivingStrategies    #-}
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
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}

module Infrastructure.Logger
  ( Config (..)
  , GConst (..)
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
  , gMkLogEntry
  , log
  , logDebug
  , logError
  , logInfo
  , logWarning
  , mkLogEntry
  , mkLogEntryLine
  , mkLogger

  , TestEnterRecordData (..)
  , TestRecordData (..)
  , TestUnitData (..)
  , TestNewtypeRecordData (..)
  , TestNewtypeCommonData (..)
  , TestNewtypeCommonOverNewtypeCommonData (..)
  , TestNewtypeRecordOverNewtypeCommonData (..)
  , TestNewtypeCommonOverNewtypeRecordData (..)
  , TestNewtypeRecordOverNewtypeRecordData (..)
  , TestNewtypeRecordNestedData (..)
  , TestSumData (..)
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
import Data.Text.Extended        ( Text )
import Data.Time                 ( UTCTime )
import GHC.Generics              ( Generic, (:*:) (..), (:+:) (..), )
import Network.HTTP.Client       ( HttpException )

import Data.Proxy                 ( Proxy (..) )
import Data.Typeable         (Typeable, typeOf, showsTypeRep )
import GHC.TypeLits (Nat, KnownNat, KnownSymbol, type (+), type (-), symbolVal, natVal)

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

class GLog f where
  glog :: Int -> f x -> Text

class GLogList f where
  gLogList :: Int -> Int -> f x -> Text

instance GLogList f => GLogList (G.M1 i c f) where
  gLogList s i (G.M1 x) = gLogList s i x

instance GLogList g => GLogList (f :+: g) where
  gLogList _ _ (L1 _) = ""
  gLogList s i (R1 x) = gLogList s i x

instance (GLog f, GLogList g)
  => GLogList ((G.S1 c f) :*: g) where
  gLogList s i ((G.M1 x) :*: y) = Text.concat
    [ "\n  |"
    , Text.replicate i "  "
    , "Index "
    , Text.showt s
    , ": "
    , glog i x
    , gLogList (s + 1) i y
    ]

instance (Generic a, GLogList (G.Rep a)) => GLogList (G.Rec0 a) where
  gLogList s i (G.K1 x) = gLogList s i $ G.from x

---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------

class GLogEnter f where
  gLogEnter :: f a -> Text

instance GLogCommon f => GLogEnter (G.D1 d f) where
  gLogEnter (G.M1 x) = gLogCommon 1 x
--instance (GLogCommon g, f ~ (G.C1 c g)) => GLogEnter (G.D1 d f) where
--  gLogEnter (G.M1 x) = gLogCommon x
--
--instance (GLogEnter g, GLogEnter f) => GLogEnter (f :+: g) where
--  gLogEnter (G.L1 x) = gLogEnter x
--  gLogEnter (G.R1 x) = gLogEnter x
--
--instance (GLogSingle f, G.Constructor c)
--  => GLogEnter (G.C1 c (G.S1 s f)) where
--  gLogEnter c@(G.M1 (G.M1 x)) = (Text.pack $ G.conName c)
--    <> ": "
--    <> gLogSingle 1 x

class GLogCommon f where
  gLogCommon :: Int -> f a -> Text

instance {-# OVERLAPPABLE #-} GLogCommon f => GLogCommon (G.D1 d f) where
  gLogCommon i (G.M1 x) = gLogCommon i x

instance GLogCommon f => GLogCommon (G.D1 d (G.C1 c (G.S1 s f))) where
  gLogCommon i (G.M1 (G.M1 (G.M1 x))) = gLogCommon i x

instance (GLogSum f, GLogSum g) => GLogCommon (f :+: g) where
  gLogCommon i (G.L1 x) = gLogSum i x
  gLogCommon i (G.R1 x) = gLogSum i x

instance {-# OVERLAPPABLE #-}
  (GLogCommon f, G.Constructor c) => GLogCommon (G.C1 c f) where
  gLogCommon i c@(G.M1 x) = (Text.pack $ G.conName c)
    <> ": "
    <> gLogCommon i x

instance (GLogNoName f, KnownSymbol c) => GLogCommon (G.C1 (G.MetaCons c p False) f) where
  gLogCommon i c@(G.M1 x) = (Text.pack $ symbolVal $ Proxy @c)
    <> ": "
    <> gLogNoName i x

instance (GLogCommon f, G.Selector s) => GLogCommon (G.S1 s f) where
  gLogCommon i s@(G.M1 x) = Text.concat
    [ "\n  |"
    , Text.replicate i "  "
    , (Text.pack $ G.selName s)
    , ": "
    , gLogCommon i x
    ]

instance {-# OVERLAPPABLE #-}
  (Generic a, GLogCommon (G.Rep a)) => GLogCommon (G.Rec0 a) where
  gLogCommon i (G.K1 x) = gLogCommon (i + 1) $ G.from x

instance GLogCommon (G.Rec0 Integer) where
  gLogCommon _ (G.K1 x) = Text.showt x

class GLogSum f where
  gLogSum :: Int -> f a -> Text

instance {-# OVERLAPPABLE #-}
  (GLogCommon f, G.Constructor c) => GLogSum (G.C1 c f) where
  gLogSum i c@(G.M1 x) = (Text.pack $ G.conName c)
    <> ": "
    <> gLogCommon i x

instance (GLogSingle f, G.Constructor c) => GLogSum (G.C1 c (G.S1 s f)) where
  gLogSum i c@(G.M1 (G.M1 x)) = (Text.pack $ G.conName c)
    <> ": "
    <> gLogSingle i x

class GLogSingle f where
  gLogSingle :: Int -> f a -> Text

instance GLogSingle f => GLogSingle (G.D1 d (G.C1 c (G.S1 s f))) where
  gLogSingle i (G.M1 (G.M1 (G.M1 x))) = gLogSingle i x

instance {-# OVERLAPPABLE #-} GLogSingle f => GLogSingle (G.D1 d f) where
  gLogSingle i (G.M1 x) = gLogSingle i x

instance (GLogSum f, GLogSum g) => GLogSingle (f :+: g) where
  gLogSingle i (G.L1 x) = gLogSum i x
  gLogSingle i (G.R1 x) = gLogSum i x

instance GLogCommon f => GLogSingle (G.C1 c f) where
  gLogSingle i (G.M1 x) = gLogCommon i x

instance {-# OVERLAPPABLE #-}
  (Generic a, GLogSingle (G.Rep a)) => GLogSingle (G.Rec0 a) where
  gLogSingle i (G.K1 x) = gLogSingle (i + 1) $ G.from x

instance GLogSingle (G.Rec0 Integer) where
  gLogSingle _ (G.K1 x) = Text.showt x

instance GLogSingle (G.Rec0 Text) where
  gLogSingle _ (G.K1 x) = x

class GLogNoName f where
  gLogNoName :: Int -> f a -> Text

instance GLogNoName f => GLogNoName (G.S1 s f) where
  gLogNoName i (G.M1 x) = gLogNoName i x

instance {-# OVERLAPPABLE #-}
  (GLogCommon g, Typeable x, g ~ (G.Rec0 x)) => GLogNoName g where
  gLogNoName i f@(G.K1 x) = Text.concat
    [ "\n  |"
    , Text.replicate i "  "
    , Text.showt $ typeOf x
    , ": "
    , gLogCommon i f
    ]
--instance (GLogEnter f, GLogEnter g) => GLogEnter (f :+: g) where
--  gLogEnter (G.L1 x) = gLogEnter x
--  gLogEnter (G.R1 x) = gLogEnter x
--
--instance {-# OVERLAPPABLE #-}
--  (GLog f, G.Constructor c) => GLogEnter (G.C1 c f) where
--  gLogEnter c@(G.M1 x) = (Text.pack $ G.conName c) <> ": " <> path
--    where path = bool gLogRecord gLogProd $ G.conIsRecord c
--          gLogRecord = glog 1 x
--          gLogProd   = glog 1 x
--
--instance G.Constructor c => GLogEnter (G.C1 c (G.S1 s G.U1)) where
--  gLogEnter x = Text.pack $ G.conName x
--
--instance (GLog f, G.Constructor c) =>
--  GLogEnter (G.C1 c (G.S1 s f)) where
--  gLogEnter c@(G.M1 (G.M1 x)) = Text.pack (G.conName c) <> glog 1 x--gLogSingle 1 x
--    where gLogSingle _ _ = undefined

class GLogNewtype f where
  gLogNewtype :: Int -> f a -> Text

instance GLog f => GLogNewtype (G.C1 c (G.S1 s f)) where
  gLogNewtype i (G.M1 (G.M1 x)) = glog i x
----------------------------------------------------------------
----------------------------------------------------------------
----------------------------------------------------------------
data Context j
  = Entry
  | Single
  | Product
  | List j

data Selector
  = Record
  | DataName

class GLogF (k :: Context Nat) (r :: Selector) (i :: Nat) f where
  gLogF :: f a -> Text

---------------------------------------------------------------
-- Entry
---------------------------------------------------------------

instance (GLogF Entry r i f, G.Constructor c)
  => GLogF Entry r i (G.C1 c f) where
    gLogF f@(G.M1 x) = mkSelector' (G.conName f) <> gLogF @Entry @r @i x

instance GLogF Entry r i G.U1 where
    gLogF _ = ""

instance GLogF Single Record i f => GLogF Entry r i (G.S1 s f) where
    gLogF (G.M1 x) = ": " <> nothingIfEmpty (gLogF @Single @Record @i x)

instance (GLogF Product r (i + 1) f, GLogF Product r (i + 1) g) =>
  GLogF Entry r i (f :*: g) where
    gLogF (x :*: y) = ": " <> nothingIfEmpty result
      where result = gLogF @Product @r @(i + 1) x
                  <> gLogF @Product @r @(i + 1) y

---------------------------------------------------
-- Single
---------------------------------------------------

instance (G.Constructor c) =>
  GLogF Single r i (G.C1 c G.U1) where
  gLogF f@(G.M1 x) = mkSelector' $ G.conName f

instance GLogF Single Record i f =>
  GLogF Single r i (G.C1 c (G.S1 s f)) where
  gLogF (G.M1 (G.M1 x)) = gLogF @Single @Record @i x

instance (GLogF Product Record (i + 1) f, GLogF Product Record (i + 1) g) =>
  GLogF Single r i (G.C1 c (f :*: g)) where
  gLogF (G.M1 (x :*: y)) = gLogF @Product @Record @(i + 1) x
    <> gLogF @Product @Record @(i + 1) y

---------------------------------------------------
-- Product
---------------------------------------------------

instance ( GLogF Product r (i + 1) f
         , GLogF Product r (i + 1) g
         , G.Constructor c
         ) => GLogF Product r i (G.C1 c (f :*: g)) where
  gLogF con@(G.M1 (x :*: y)) = Text.concat
    [ mkSelector' $ G.conName con
    , ": "
    , nothingIfEmpty $ resultF <> resultG
    ]
    where resultF = gLogF @Product @r @(i + 1) x
          resultG = gLogF @Product @r @(i + 1) y

instance ( GLogF Product r i f
         , GLogF Product r i g
         ) => GLogF Product r i (f :*: g) where
  gLogF (x :*: y) = Text.concat
    [ gLogF @Product @r @i x
    , gLogF @Product @r @i y
    ]

instance (GLogF Single r i f, G.Constructor c) =>
  GLogF Product r i (G.C1 c (G.S1 s f)) where
  gLogF con@(G.M1 (G.M1 x)) = Text.concat
    [ mkSelector' $ G.conName con
    , ": "
    , nothingIfEmpty $ gLogF @Single @r @i x
    ]

instance G.Constructor c => GLogF Product r i (G.C1 c G.U1) where
  gLogF = Text.pack . G.conName

instance (GLogF Product Record i f, KnownSymbol n, KnownNat i) =>
  GLogF Product r i (G.S1 (G.MetaSel (Just n) a b c) f) where
  gLogF (G.M1 x) = bool line "" $ Text.null result
    where result = gLogF @Product @Record @i x
          line   = Text.concat
            [ "\n  |"
            , Text.replicate (fromInteger $ natVal $ Proxy @i) "  "
            , mkSelector' $ symbolVal $ Proxy @n
            , ": "
            , result
            ]

instance GLogF Product DataName i f =>
  GLogF Product r i (G.S1 (G.MetaSel Nothing a b c) f) where
  gLogF (G.M1 x) = gLogF @Product @DataName @i x

---------------------------------------------------
-- Common
---------------------------------------------------

instance --{-# OVERLAPPABLE #-}
  GLogF k r i f => GLogF k r i (G.D1 d f) where
  gLogF (G.M1 x) = gLogF @k @r @i x

instance {-# OVERLAPPABLE #-}
  (GLogF k r i f, GLogF k r i g) =>
  GLogF k r i (f :+: g) where
    gLogF (G.L1 x) = gLogF @k @r @i x
    gLogF (G.R1 x) = gLogF @k @r @i x

instance
  (GLogF k Record i (G.Rec0 a), Typeable a, KnownNat i) =>
  GLogF k DataName i (G.Rec0 a) where
  gLogF f@(G.K1 x) = bool line "" $ Text.null result
    where result = gLogF @k @Record @i f
          line   = Text.concat
            [ "\n  |"
            , Text.replicate (fromInteger $ natVal $ Proxy @i) "  "
            ,Text.showt $ typeOf x
            , ": "
            , result
            ]

instance {-# OVERLAPPABLE #-}
  (Generic a, GLogF k Record i (G.Rep a)) =>
  GLogF k Record i (G.Rec0 a) where
  gLogF (G.K1 x) = gLogF @k @Record @i $ G.from x

instance GLogF k Record i (G.Rec0 Int) where
  gLogF (G.K1 x) = Text.showt x

instance GLogF k Record i (G.Rec0 Integer) where
  gLogF (G.K1 x) = Text.showt x

instance GLogF k Record i (G.Rec0 Double) where
  gLogF (G.K1 x) = Text.showt x

instance GLogF k Record i (G.Rec0 Text) where
  gLogF (G.K1 x) = x

instance GLogF k Record i (G.Rec0 Aeson.Value) where
  gLogF (G.K1 x) = Text.showt $ typeOf x

instance GLogF k Record i (G.Rec0 a) =>
         GLogF k Record i (G.Rec0 (Maybe a)) where
  gLogF (G.K1 (Just x)) = gLogF @k @Record @i @(G.Rec0 a) $ G.K1 x
  gLogF (G.K1 Nothing)  = ""

instance (GLogL (i + 1) (G.Rep [a]), Typeable a) =>
  GLogF k Record i (G.Rec0 [a]) where
  gLogF (G.K1 x) = Text.concat
    [ Text.showt $ typeOf x
    , ": "
    , gLogL @(i + 1) 0 $ G.from x
    ]

---------------------------------------------------
-- List
---------------------------------------------------
class GLogL (i :: Nat) f where
  gLogL :: Int -> f a -> Text

instance GLogL i f => GLogL i (G.M1 a b f) where
  gLogL index (G.M1 x) = gLogL @i index x

instance GLogL i g => GLogL i (f :+: g) where
  gLogL _ (G.L1 _) = ""
  gLogL index (G.R1 x) = gLogL @i index x
--
--instance GLogF (List j) Record i f =>
--  GLogF (List j) Record i (G.C1 c f) where
--  gLogF (G.M1 x) = gLogF @(List j) @Record @i x
--
--instance GLogF (List j) Record i f =>
--  GLogF (List j) Record i (G.S1 s f) where
--  gLogF (G.M1 x) = gLogF @(List j) @Record @i x
--
instance ( GLogF Entry Record i f
         , GLogL i g
         , KnownNat i
         ) => GLogL i ((G.S1 s f) :*: g) where
  gLogL index ((G.M1 x) :*: xs) = Text.concat
    [ "\n  |"
    , Text.replicate (fromInteger $ natVal $ Proxy @i) "  "
    , "Index "
    , Text.showt index
    , ": "
    , nothingIfEmpty $ gLogF @Entry @Record @i x
    , gLogL @i index xs
    ]
--
instance -- {-# OVERLAPPING #-}
  (Generic a, GLogL i (G.Rep a)) =>
  GLogL i (G.Rec0 a) where
  gLogL index (G.K1 x) = gLogL @i (index + 1) $ G.from x

--instance
--  (GLogF Entry Record i (G.Rec0 a)) =>
--  GLogF (List j) Record i (G.Rec0 a) where
--  gLogF x = gLogF @Entry @Record @i x
--instance (GLog f, GLogList g)
--  => GLogList ((G.S1 c f) :*: g) where
--  gLogList s i ((G.M1 x) :*: y) = Text.concat
--    [ "\n  |"
--    , Text.replicate i "  "
--    , "Index "
--    , Text.showt s
--    , ": "
--    , glog i x
--    , gLogList (s + 1) i y
--    ]
-- D1

--class GLogF (c :: Context) (g :: RecordStatus) (i :: Nat) f where
--  gLogF :: f x -> Text
--
--instance GLogF Common s i f => GLogF Enter s i (G.D1 d f) where
--  gLogF (G.M1 x) = gLogF @Common @s @i x
--
--instance GLogF Single Added (i - 1) f
--  => GLogF Single k i (G.D1 d (G.C1 c (G.S1 s f))) where
--  gLogF (G.M1 (G.M1 (G.M1 x))) = gLogF @Single @Added @(i - 1) x
--
---- :+:
--
--instance (GLogF Sum r i f, GLogF Sum r i g)
--  => GLogF c r i (f :+: g) where
--  gLogF (G.L1 x) = gLogF @Sum @r @i x
--  gLogF (G.R1 x) = gLogF @Sum @r @i x
--
---- C1
--
--instance (GLogF Single Added (i - 1) f, G.Constructor c)
--  => GLogF Sum NotAdded i (G.C1 c (G.S1 s f)) where
--  gLogF con@(G.M1 (G.M1 x)) = Text.concat
--    [ Text.pack $ G.conName con
--    , ": "
--    , gLogF @Single @Added @(i - 1) x
--    ]
--
--instance --{-# OVERLAPPING #-}
--  (GLogF Product Added i f, GLogF Product Added i g, KnownSymbol a)
--  => GLogF Sum k i (G.C1 (G.MetaCons a b True) (f :*: g)) where
--  gLogF (G.M1 (x :*: y)) = Text.concat
--    [ Text.showt $ symbolVal $ Proxy @a
--    , ": "
--    , gLogF @Product @Added @i x
--    , gLogF @Product @Added @i y
--    ]
--
--instance --{-# OVERLAPPING #-}
--  (GLogF Product NotAdded i f, GLogF Product NotAdded i g, KnownSymbol a)
--  => GLogF Sum k i (G.C1 (G.MetaCons a b False) (f :*: g)) where
--  gLogF (G.M1 (x :*: y)) = Text.concat
--    [ Text.showt $ symbolVal $ Proxy @a
--    , ": "
--    , gLogF @Product @NotAdded @i x
--    , gLogF @Product @NotAdded @i y
--    ]
--instance GLogF Common Added (i - 1) f
--  => GLogF Common r i
--  (G.D1 (G.MetaData q w e True) (G.C1 c (G.S1 s f))) where
--  gLogF (G.M1 (G.M1 (G.M1 x))) = gLogF @Common @Added @(i - 1) x
--
--instance-- {-# OVERLAPPABLE #-}
--  (GLogF Common NotAdded i f, KnownSymbol a)
--  => GLogF Common s i (G.C1 (G.MetaCons a b False) f) where
--  gLogF (G.M1 x) = Text.pack (symbolVal $ Proxy @a)
--    <> gLogF @Common @NotAdded @i x
--
--instance --{-# Overlappable #-}
--  (GLogF Common Added i f, KnownSymbol a)
--  => GLogF Common s i (G.C1 (G.MetaCons a b True) f) where
--  gLogF c@(G.M1 x) = Text.pack (symbolVal $ Proxy @a)
--    <> gLogF @Common @Added @i x
--
---- S1
--
--instance (GLogF k Added i f, G.Selector s, KnownNat i)
--  => GLogF k Added i (G.S1 s f) where
--  gLogF s@(G.M1 x) = Text.concat
--    [ "\n  |"
--    , Text.replicate (fromInteger $ natVal $ Proxy @i) "  "
--    , Text.pack $ G.selName s
--    , ": "
--    , gLogF @k @Added @i x
--    ]
--
--instance (GLogF k Added i f, f ~ G.Rec0 a, Typeable a, KnownNat i)
--  => GLogF k NotAdded i (G.S1 s f) where
--  gLogF (G.M1 f@(G.K1 x)) = Text.concat
--    [ ":\n  |"
--    , Text.replicate (fromInteger $ natVal $ Proxy @i) "  "
--    , Text.showt $ typeOf x
--    , ": "
--    , gLogF @k @Added @i f
--    ]
--
---- Rec0
--
--instance {-# OVERLAPPABLE #-}
--  (Generic a, GLogF k NotAdded (i + 1) (G.Rep a))
--  => GLogF k Added i (G.Rec0 a) where
--  gLogF (G.K1 x) = gLogF @k @NotAdded @(i + 1) $ G.from x
--
--instance GLogF k Added i (G.Rec0 Integer) where
--  gLogF (G.K1 x) = Text.showt x

--instance GLogF k NotAdded i (G.Rec0 Integer) where
--  gLogF (G.K1 x) = Text.showt x

--instance f ~ G.Rec0 a
--  => GLogF k NotAdded i f where
--  gLogF x = ":\n  |" <> Text.showt (typeOf $ Proxy @a) <> gLogF @k @Added @i x
--instance (GLogF Sum s i f, GLogF Sum s i g)
--  => GLogF Common s i (f :+: g) where
--  gLogF (G.L1 x) = gLogF @Sum @s @i x
--  gLogF (G.R1 x) = gLogF @Sum @s @i x

--instance {-# OVERLAPPABLE #-} (GLogF Common Added i f, G.Constructor c)
--  => GLogF Common Added i (G.C1 c f) where
--  gLogF c@(G.M1 x) = (Text.pack $ G.conName c) <> path
--    where path | G.conIsRecord c = gLogF @Common @Added    @i x
--               | otherwise       = gLogF @Common @NotAdded @i x

--instance (GLogF Common NotAdded i f)
--  => GLogF Common Added i (G.C1 (G.MetaCons a b False) f) where
--  gLogF c@(G.M1 x) = (Text.pack $ G.conName c) <> path
--    where path | G.conIsRecord c = gLogF @Common @NotAdded    @i x
--
--instance (GLogF Common NotAdded i f, KnownNat i, G.Selector s)
--  => GLogF Common NotAdded i (G.S1 s f) where
--  gLogF s@(G.M1 x) = Text.concat
--    [ "\n  |"
--    , Text.replicate (fromInteger $ natVal $ Proxy @i) "  "
--    , Text.pack $ G.selName s
--    , gLogF @Common @NotAdded @i x
--    ]
--
--instance (GLogF Common Added i f, KnownNat i, G.Selector s)
--  => GLogF Common Added i (G.S1 s f) where
--  gLogF s@(G.M1 x) = Text.concat
--    [ "\n  |"
--    , Text.replicate (fromInteger $ natVal $ Proxy @i) "  "
--    , Text.pack $ G.selName s
--    , gLogF @Common @Added @i x
--    ]
----instance (Generic a, GLogF Common NotAdded j (G.Rep a))
----  => GLogF Common Added i (G.Rec0 a) where
----  gLogF (G.K1 x) = ": " <> gLogF @Common @NotAdded @(i + 1) (G.from x)
--
--instance GLogF Common Added i (G.Rec0 Integer) where
--  gLogF (G.K1 x) = ": " <> Text.showt x
--
--instance GLogF Common NotAdded i (G.Rec0 Integer) where
--  gLogF (G.K1 x) = Text.showt (typeOf x) <> ": " <> Text.showt x
--class GLogSingle f where
--  gLogSingle 
-- TYPES AND INSTANCES -----------------------------------------------------

type Lock = MVar ()

type HasLogger r m =
  ( Has (Logger m) r
  , MonadReader r m
  , MonadTime m
  , MonadLogger m
  )

instance GLogNewtype f => GLog (G.D1 (G.MetaData n m p True) f) where
  glog i (G.M1 x) = gLogNewtype i x

instance {-# OVERLAPPABLE #-} GLog f => GLog (G.D1 c f) where
  glog i (G.M1 x) = glog i x

instance (GLog a, GLog b) => GLog (a :+: b) where
  glog i (G.L1 x) = glog i x
  glog i (G.R1 x) = glog i x

instance {-# OVERLAPPABLE #-}
  (GLog f, G.Constructor c) => GLog (G.C1 c f) where
  glog i c@(G.M1 x) = Text.pack (G.conName c)
    <> (bool "" ":" $ G.conIsRecord c)
    <> glog i x

instance (GLog f, G.Constructor c, G.Selector s)
  => GLog (G.C1 c (G.S1 s f)) where
  glog i c@(G.M1 (G.M1 x)) = glog (i - 1) x

instance (GLog a, GLog b) => GLog (a :*: b) where
  glog i (x :*: y) = glog i x <> glog i y

instance (GLog f, G.Selector c) => GLog (G.S1 c f) where
  glog i c@(G.M1 x)
    | Text.null field = ""
    | otherwise       = "\n  |"
                     <> (Text.replicate i "  ")
                     <> Text.pack (mkSelector $ G.selName c)
                     <> field
    where field = glog i x

instance {-# OVERLAPPABLE #-}
  (Generic f, GLog (G.Rep f)) => GLog (G.Rec0 f) where
  glog i (G.K1 x) = glog (i + 1) $ G.from x --gMkLogEntry' (i + 1) x

--instance GLog (G.Rec0 a) => GLog (G.Rec0 (Maybe a)) where
--  glog i (G.K1 x) = maybe "" (glog @(G.Rec0 a) i . G.K1) x

instance (GLogList (G.Rep [a]), Typeable a) => GLog (G.Rec0 [a]) where
  glog i (G.K1 xs) = (Text.showt $ typeOf xs)
    <> gLogList 0 (i + 1) (G.from xs)

instance GLog (G.Rec0 Integer) where
  glog _ (G.K1 x) = Text.showt x

instance GLog (G.Rec0 Aeson.Value) where
  glog _ (G.K1 _) = "Aeson Value"

instance GLog (G.Rec0 Int) where
  glog _ (G.K1 x) = Text.showt x

instance GLog (G.Rec0 Double) where
  glog _ (G.K1 x) = Text.showt x

instance GLog (G.Rec0 Text) where
  glog _ (G.K1 x) = x

instance GLog (G.Rec0 String) where
  glog _ (G.K1 x) = Text.pack x

instance GLog G.U1 where glog _ _ = ""

instance Loggable HttpException where
  logData e = logError $ mkLogEntry
    "HttpException:"
    [("Content", Text.showt e)]

newtype LogError a = LogError a

instance (Generic a, GLog (G.Rep a)) => Loggable (LogError a) where
  logData (LogError x) = logError $ gMkLogEntry' 1 x

newtype LogDebug a = LogDebug a

--instance (Generic a, GLog (G.Rep a)) => Loggable (LogDebug a) where
--  logData (LogDebug x) = logDebug $ gMkLogEntry' 1 x

instance (Generic a, GLogF Entry Record 0 (G.Rep a))
  => Loggable (LogDebug a) where
  logData (LogDebug x) = logDebug $ gLogF @Entry @Record @0 $ G.from x

newtype LogTest a = LogTest a

instance (Generic a, GLogF Entry Record 0 (G.Rep a))
  => Loggable (LogTest a) where
  logData (LogTest x) = logDebug $ gLogF @Entry @Record @0 $ G.from x

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
mkLogEntryLine (key, value) = "\n | " <> key <> ": " <> value

gMkLogEntry' :: (Generic a, GLog (G.Rep a)) => Int -> a -> Text
gMkLogEntry' i = glog i . G.from

gMkLogEntry :: (Generic a, GLogEnter (G.Rep a)) => a -> Text
gMkLogEntry = gLogEnter . G.from

mkSelector :: String -> String
mkSelector [] = "Field: "
mkSelector s@(x:xs)
  | all isLower s = toUpper x : xs <> ": "
  | otherwise     = (toUpper l :) $ foldr go ": " ls
  where (l:ls) = dropWhile isLower s
        go x xs | isUpper x = ' ' : x : xs
                | otherwise = x : xs

mkSelector' :: String -> Text
mkSelector' s = Text.pack $ dropPrefix s
  where dropPrefix (x:xs)
          | all isLower s = toUpper x : xs
          | otherwise     = (toUpper l :) $ foldr go "" ls
        go x xs
          | isUpper x = ' ' : x : xs
          | otherwise = x : xs
        (l:ls) = dropWhile isLower s

nothingIfEmpty :: Text -> Text
nothingIfEmpty t = bool t "Nothing" $ Text.null t

newtype TestNewtypeCommonData = TestNewtypeCommonCons Integer
  deriving stock Generic
  deriving Loggable via LogTest TestNewtypeCommonData

newtype TestNewtypeRecordData = TestNewtypeRecordCons
  { testNewtypeRecordSelector :: Integer }
  deriving stock Generic
  deriving Loggable via LogTest TestNewtypeRecordData

data TestUnitData = TestUnitCons
  deriving stock Generic
  deriving Loggable via LogTest TestUnitData

newtype TestNewtypeCommonOverNewtypeCommonData
  = TestNewtypeCommonOverNewtypeCommonCons TestNewtypeCommonData
  deriving stock Generic
  deriving Loggable via LogTest TestNewtypeCommonOverNewtypeCommonData

newtype TestNewtypeCommonOverNewtypeRecordData
  = TestNewtypeCommonOverNewtypeRecordCons TestNewtypeRecordData
  deriving stock Generic
  deriving Loggable via LogTest TestNewtypeCommonOverNewtypeRecordData

newtype TestNewtypeRecordOverNewtypeRecordData
  = TestNewtypeRecordOverNewtypeRecordCons
    { testNewtypeRecordOverNewtypeRecordSelector :: TestNewtypeRecordData }
  deriving stock Generic
  deriving Loggable via LogTest TestNewtypeRecordOverNewtypeRecordData

newtype TestNewtypeRecordOverNewtypeCommonData
  = TestNewtypeRecordOverNewtypeCommonCons
    { testNewtypeRecordOverNewtypeCommonSelector :: TestNewtypeCommonData }
  deriving stock Generic
  deriving Loggable via LogTest TestNewtypeRecordOverNewtypeCommonData

newtype TestNewtypeRecordNestedData
  = TestNewtypeRecordNestedCons
    { testNewtypeRecordNestedSelector
      :: TestNewtypeRecordOverNewtypeRecordData }
  deriving stock Generic
  deriving Loggable via LogTest TestNewtypeRecordNestedData

data TestRecordData = TestRecordCons
  { testRecordFieldNewtypeCommon :: TestNewtypeCommonData
  , testRecordFieldNewtypeRecord :: TestNewtypeRecordData
  , testRecordFieldListOfNewtypeCommon :: [TestNewtypeCommonData]
  , testRecordFieldListOfNewtypeRecord :: [TestNewtypeRecordData]
  } deriving stock Generic
    deriving Loggable via LogTest TestRecordData

data TestSumData
  = TestSumSingleNewtypeCommonCons TestNewtypeCommonData
  | TestSumSingleNewtypeRecordCons TestNewtypeRecordData
  | TestSumNewtypeRecordAndNewtypeCommonCons
    TestNewtypeRecordData
    TestNewtypeCommonData
  | TestSumOfProductsCons
    TestRecordData
    TestRecordData
    TestUnitData
    TestNewtypeCommonData
    TestNewtypeRecordData
    Integer
  | TestSumSingleListOfNewtypeCommonCons [TestNewtypeCommonData]
  | TestSumSingleListOfNewtypeRecordCons [TestNewtypeRecordData]
  | TestSumSingleListOfRecordCons [TestRecordData]
  deriving stock Generic
  deriving Loggable via LogTest TestSumData

data TestEnterRecordData = TestEnterRecordCons
  { testEnterFieldNewtypeCommon :: TestNewtypeCommonData
  , testEnterFieldNewtypeRecord :: TestNewtypeRecordData
  , testEnterFieldListOfNewtypeCommon :: [TestNewtypeCommonData]
  , testEnterFieldListOfNewtypeRecord :: [TestNewtypeRecordData]
  , testEnterFieldRecord :: TestRecordData
  , testEnterFieldListOfRecord :: [TestRecordData]
  , testEnterRecordFieldSumSingle :: TestSumData
  , testEnterRecordFieldSumList :: TestSumData
  , testEnterRecordFieldListOfSum :: [TestSumData]
  } deriving stock Generic
    deriving Loggable via LogTest TestEnterRecordData


