{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Infrastructure.Logger.LogRepresentation where

-- IMPORTS -----------------------------------------------------------------

import Data.Aeson         ( Value )
import Data.Bool          ( bool )
import Data.Char          ( isLower, isUpper, toUpper )
import Data.Proxy         ( Proxy (..) )
import Data.Text.Extended ( Text )
import Data.Typeable      ( Typeable, typeOf )
import GHC.Generics       ( Generic, (:*:) (..), (:+:) (..), )
import GHC.TypeLits       ( Nat, KnownNat, KnownSymbol, type (+)
                          , symbolVal, natVal
                          )

import qualified Data.Text.Extended as Text
import qualified GHC.Generics       as G

-- CLASSES -----------------------------------------------------------------

class GLog (k :: Context) (r :: DataNameState) (i :: Nat) f where
  gLog :: f a -> Text

class GLogList (i :: Nat) f where
  gLogList :: Int -> f a -> Text

-- TYPES -------------------------------------------------------------------

data Context
  = Entry
  | Single
  | Product

data DataNameState
  = SkipDataName
  | AddDataName

-- INSTANCES ---------------------------------------------------------------

-- Common

instance GLog k r i f => GLog k r i (G.D1 d f) where
  gLog (G.M1 x) = gLog @k @r @i x

instance (GLog k r i f, GLog k r i g) => GLog k r i (f :+: g) where
  gLog (G.L1 x) = gLog @k @r @i x
  gLog (G.R1 x) = gLog @k @r @i x

instance
  ( GLog k SkipDataName i (G.Rec0 a)
  , Typeable a
  , KnownNat i
  ) => GLog k AddDataName i (G.Rec0 a) where
  gLog f@(G.K1 x) = bool line "" $ Text.null result
    where
      result = gLog @k @SkipDataName @i f

      line   = Text.concat
            [ "\n  |"
            , Text.replicate (fromInteger $ natVal $ Proxy @i) "  "
            ,Text.showt $ typeOf x
            , ": "
            , result
            ]

instance {-# OVERLAPPABLE #-}
  ( Generic a
  , GLog k SkipDataName i (G.Rep a)
  ) => GLog k SkipDataName i (G.Rec0 a) where
  gLog (G.K1 x) = gLog @k @SkipDataName @i $ G.from x

instance GLog k SkipDataName i (G.Rec0 Int) where
  gLog (G.K1 x) = Text.showt x

instance GLog k SkipDataName i (G.Rec0 Integer) where
  gLog (G.K1 x) = Text.showt x

instance GLog k SkipDataName i (G.Rec0 Double) where
  gLog (G.K1 x) = Text.showt x

instance GLog k SkipDataName i (G.Rec0 Text) where
  gLog (G.K1 x) = x

instance GLog k SkipDataName i (G.Rec0 Value) where
  gLog (G.K1 x) = Text.showt $ typeOf x

instance GLog k SkipDataName i (G.Rec0 a) =>
         GLog k SkipDataName i (G.Rec0 (Maybe a)) where
  gLog (G.K1 (Just x)) = gLog @k @SkipDataName @i @(G.Rec0 a) $ G.K1 x
  gLog (G.K1 Nothing)  = ""

instance
  ( GLogList (i + 1) (G.Rep [a])
  , Typeable a
  ) => GLog k SkipDataName i (G.Rec0 [a]) where

  gLog (G.K1 x) = Text.concat
    [ Text.showt $ typeOf x
    , " (length: "
    , Text.showt $ length x
    , "): "
    , gLogList @(i + 1) 0 $ G.from x
    ]

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
