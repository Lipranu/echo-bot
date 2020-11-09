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
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DerivingStrategies      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE RecordWildCards      #-}

module Infrastructure.Logger.LogRepresentation
  ( LogRepresentation (..)
  , GLogRepresentation
  , gLogRepresentation
  , mkLogRepresentation
  , mkLogRepresentationLine
  , l1,l2,l3
  , lttot
  , t
  , t'
  , d
  , LogLayout (..)
  ) where

-- IMPORTS -----------------------------------------------------------------

import Control.Applicative ( (<|>) )
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
import qualified Data.Text.IO as TextIO
import qualified GHC.Generics       as G

-- CLASSES -----------------------------------------------------------------
t = TextIO.putStrLn $ logLay d -- $ TestG $ TestF 1
t' = TextIO.putStrLn $ logLay d' -- $ TestG $ TestF 1
class LogRepresentation a where
  logRepresentation :: a -> Text

  default logRepresentation :: GLogRepresentation a => a -> Text
  logRepresentation = gLogRepresentation

class GLog (k :: Context) (r :: DataNameState) (i :: Nat) f where
  gLog :: f a -> Text

class GLogList (i :: Nat) f where
  gLogList :: Int -> f a -> Text

-- TYPES -------------------------------------------------------------------

type GLogRepresentation a = (Generic a, GLog Entry SkipDataName 0 (G.Rep a))

data Context
  = Entry
  | Single
  | Product

data DataNameState
  = SkipDataName
  | AddDataName

-- INSTANCES ---------------------------------------------------------------

instance LogRepresentation a => LogRepresentation [a] where
  logRepresentation = Text.concat . zipWith zipper [0 .. ]
    where zipper i s = Text.concat
            [ "Index "
            , Text.showt i
            , ": "
            , logRepresentation s
            , "\n"
            ]

instance LogRepresentation Integer where logRepresentation = Text.showt
data Test = Test
  { testA :: Integer
  , testB :: Double
  , testC :: Text
  } deriving stock Generic
    deriving anyclass LogRepresentation

data Result = Empty | Unit | Some Text deriving (Show, Eq)

instance Semigroup Result where
  r1 <> r2 = case r1 of
    Empty -> r2
    _     -> r1

instance Monoid Result where
  mempty = Empty

data Layout = Layout
  { title  :: Text
  , result :: Text
  , fields :: [Layout]
  } deriving stock (Eq, Show)

instance Semigroup Layout where
  Layout t1 r1 f1 <> Layout t2 r2 f2 =
    let title  = bool t1 t2 $ Text.null t1
        result = bool r1 r2 $ Text.null r1
     in Layout title result $ f1 <> f2

instance Monoid Layout where
  mempty = Layout mempty mempty mempty

class GLogF f where
  gLogF :: f a -> Layout -> Layout

instance GLogF f => GLogF (G.D1 d f) where
  gLogF (G.M1 x) = gLogF x

instance (GLogF f, GLogF g) => GLogF (f :+: g) where
  gLogF (G.L1 x) = gLogF x
  gLogF (G.R1 x) = gLogF x

instance (GLogF f, G.Constructor c) => GLogF (G.C1 c f) where
  gLogF c@(G.M1 x) l = l <> gLogF x mempty
    { title = Text.pack $ G.conName c }

instance GLogF G.U1 where
  gLogF _ l = l { result = title l }

instance (GLogF (G.S1 s f), GLogF (g :*: h)) =>
  GLogF ((G.S1 s f) :*: (g :*: h)) where
  gLogF (x :*: y) l = gLogF y l `inject` gLogF x mempty

instance (GLogF (f :*: g), GLogF (h :*: k)) =>
  GLogF ((f :*: g) :*: (h :*: k)) where
  gLogF (x :*: y) l = gLogF x l <> gLogF y l

instance (GLogF (G.S1 s f), GLogF (G.S1 s' g)) =>
  GLogF ((G.S1 s f) :*: (G.S1 s' g)) where
  gLogF (x :*: y) l = l `inject` gLogF y mempty `inject` gLogF x mempty

--instance (GLogF f, GLogF g, G.Selector s) => GLogF (f :*: G.U1) where
--  gLogF (x :*: y) l = inject (inject l $ gLogF y mempty) $ gLogF x mempty
    --l -< gLogF y mempty -< gLogF x mempty
--    where last = inject l $ gLogF y mempty
--    l { fields = gLogF x mempty
--                                 : gLogF y mempty
--                                 : fields l
--                        }
--    where inject l1 l2 = l1 { fields = l2 : fields l1 }
--          project l = l : fields l
--          i = inject l $ gLogF y mempty

(-<) :: Layout -> Layout -> Layout
(-<) l1 l2 = l1 { fields = l2 : fields l1 }

inject :: Layout -> Layout -> Layout
inject l1 l2 = l1 { fields = l2 : fields l1 }
--instance {-# OVERLAPPING #-} (GLogF f, GLogF (G.S1 s a)) => GLogF (f :*: (G.S1 s a)) where
--  gLogF (x :*: y) l = l { fields = fields l <> [gLogF x mempty] <> [gLogF y mempty] }

instance (GLogF f, G.Selector s) => GLogF (G.S1 s f) where
  gLogF s@(G.M1 x) l = gLogF x $ l <> mempty
    { title = Text.pack $ G.selName s }

instance LogLayout a => GLogF (G.Rec0 a) where
  gLogF (G.K1 x) l = l <> logLayout x

class LogLayout a where
  logLayout :: a -> Layout
--  default logLayout (Generic a, GLogF (G.Rep a)) => a -> Layout
--  logLayout = g

instance {-# OVERLAPPABLE #-}
  (Generic a, GLogF (G.Rep a)) => LogLayout a where
  logLayout x = gLogF (G.from x) mempty

logLay :: (Generic a, GLogF (G.Rep a)) => a -> Text
logLay x = lttot 0 $ gLogF (G.from x) mempty

instance LogLayout Integer where
  logLayout x = mempty { result = Text.showt x }

data TestG = TestG { testG :: TestF } deriving (Show, Generic)
data TestF = TestF { testF :: Integer } deriving (Show, Generic)

data TestD = TestD
  { testDG :: TestG
  , testDF :: TestF
  , testI :: Integer
  , testDU :: TestU
  } deriving (Show, Generic)

data TestUU = TestUU
  { testU1 :: TestU
  , testU2 :: TestU
  , testU3 :: TestU
  , testU4 :: TestU
  , testU5 :: TestU
  , testU6 :: TestU
  , testU7 :: TestU
  } deriving (Show, Generic)
d' = TestUU U1 U2 U3 U1 U2 U3 U1
data TestU = U1 | U2 | U3 deriving (Show, Generic)
d = TestD (TestG $ TestF 1) (TestF 2) 3 U1

l1 = Layout "l1" "r1" [il1, il2, il3]
l2 = Layout "l2" "r2" [il2]
l3 = Layout "l2" "r3" [il3]
--t x y = x <|> y
il1 = Layout "il1" "ir1" [iil1]
il2 = Layout "il2" "ir2" [iil2, iil3]
il3 = Layout "il3" "ir3" []

iil1 = Layout "iil1" "iir1" []
iil2 = Layout "iil2" "iir2" []
iil3 = Layout "iil3" "iir3" []

lttot :: Int -> Layout -> Text
lttot i Layout {..} = Text.concat
  [ r
  , "\n"
  , f
  ]
  where f = foldMap ((<>) ("  |  " <> (Text.replicate i "  ")) . lttot (succ i) ) fields
        r = bool (title <> ": " <> result) result $ title == result
-- Common ------------------------------------------------------------------

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

-- Entry -------------------------------------------------------------------

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

-- Single ------------------------------------------------------------------

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

-- Product -----------------------------------------------------------------

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

-- List --------------------------------------------------------------------

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

-- FUNCTIONS ---------------------------------------------------------------

gLogRepresentation :: GLogRepresentation a => a -> Text
gLogRepresentation = gLog @Entry @SkipDataName @0 . G.from

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

mkLogRepresentation :: Text -> [(Text, Text)] -> Text
mkLogRepresentation text lines = text
  <> Text.concat (mkLogRepresentationLine <$> lines)

mkLogRepresentationLine :: (Text, Text) -> Text
mkLogRepresentationLine (key, value) = "\n  |  " <> key <> ": " <> value
