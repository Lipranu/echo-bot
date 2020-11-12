{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Logger.LogText
  ( LogText (..)
  , ToLayout (..)
  , layoutToText
  , mkLogText
  , mkLogTextLine
  ) where

-- IMPORTS -----------------------------------------------------------------

import Data.Aeson         ( Value )
import Data.Bool          ( bool )
import Data.Char          ( isLower, isUpper, toUpper )
import Data.Foldable      ( foldl' )
import Data.Text.Extended ( Text )
import Data.Typeable      ( Typeable, typeOf )
import GHC.Generics       ( Generic, (:*:) (..), (:+:) (..), )

import qualified Data.Text.Extended as Text
import qualified GHC.Generics       as G

-- CLASSES -----------------------------------------------------------------

class LogText a where
  logText :: a -> Text

  default logText :: ToLayout a => a -> Text
  logText = layoutToText . toLayout

-- GLayout -----------------------------------------------------------------

class GLayout f where
  gLayout :: f a -> Layout -> Layout

instance GLayout f => GLayout (G.D1 d f) where
  gLayout (G.M1 x) = gLayout x

instance (GLayout f, GLayout g) => GLayout (f :+: g) where
  gLayout (G.L1 x) = gLayout x
  gLayout (G.R1 x) = gLayout x

instance (GLayout f, G.Constructor c) => GLayout (G.C1 c f) where
  gLayout c@(G.M1 x) l = l <> gLayout x mempty
    { title = toCapitalCase $ G.conName c }

instance (GLayout (G.S1 s f), GLayout (g :*: h)) =>
  GLayout ((G.S1 s f) :*: (g :*: h)) where
  gLayout (x :*: y) l = gLayout y l `inject` gLayout x mempty

instance (GLayout (f :*: g), GLayout (h :*: k)) =>
  GLayout ((f :*: g) :*: (h :*: k)) where
  gLayout (x :*: y) l = gLayout x l <> gLayout y l

instance (GLayout (G.S1 s f), GLayout (G.S1 s' g)) =>
  GLayout ((G.S1 s f) :*: (G.S1 s' g)) where
  gLayout (x :*: y) l = l `inject` gLayout y mempty `inject` gLayout x mempty

instance (GLayout f, G.Selector s) => GLayout (G.S1 s f) where
  gLayout s@(G.M1 x) l = l <> gLayout x mempty
    { title = toCapitalCase $ G.selName s }

instance GLayout G.U1 where
  gLayout _ l = l { result = title l }

instance ToLayout a => GLayout (G.Rec0 a) where
  gLayout (G.K1 x) l = l <> toLayout x

-- ToLayout ----------------------------------------------------------------

class ToLayout a where
  toLayout :: a -> Layout

instance {-# OVERLAPPABLE #-}
  (Generic a, GLayout (G.Rep a)) => ToLayout a where
  toLayout x = gLayout (G.from x) mempty

instance (Typeable a, ToLayout a) => ToLayout [a] where
  toLayout xs = mempty { title = Text.showt $ typeOf xs, fields = mkFields }
    where mkFields     = zipWith addIndex [1 .. ] $ toLayout <$> xs
          addIndex i l = mempty { title = "Index " <> Text.showt i } <> l

instance (Typeable a, ToLayout a) => ToLayout (Maybe a) where
  toLayout x = mempty { title = Text.showt $ typeOf x }
    <> maybe mempty toLayout x

instance ToLayout Text where
  toLayout x = mempty { title = Text.showt $ typeOf x, result = x }

deriving via (ShowLayout Int)     instance ToLayout Int
deriving via (ShowLayout Integer) instance ToLayout Integer
deriving via (ShowLayout Double)  instance ToLayout Double
deriving via (ShowLayout Value)   instance ToLayout Value

-- TYPES -------------------------------------------------------------------

data Layout = Layout
  { title  :: Text
  , result :: Text
  , fields :: [Layout]
  } deriving stock (Eq, Show)

instance Semigroup Layout where
  Layout t1 r1 f1 <> Layout t2 r2 f2 = Layout title result $ f1 <> f2
    where title  = secondIfEmpty t1 t2
          result = secondIfEmpty r1 r2
          secondIfEmpty x y = bool x y $ Text.null x

instance Monoid Layout where
  mempty = Layout mempty mempty mempty

newtype ShowLayout a = ShowLayout a

instance (Show a, Typeable a) => ToLayout (ShowLayout a) where
  toLayout (ShowLayout x) = mempty
    { title  = Text.showt $ typeOf x
    , result = Text.showt x
    }

-- FUNCTIONS ---------------------------------------------------------------

inject :: Layout -> Layout -> Layout
inject l1 l2 = l1 { fields = l2 : fields l1 }

toCapitalCase :: String -> Text
toCapitalCase [] = mempty
toCapitalCase xs = Text.pack $ dropPrefix xs
  where dropPrefix (y:ys)
          | all isLower xs = toUpper y : ys
          | otherwise      = (toUpper l :) $ foldr go mempty ls
        go z zs
          | isUpper z = ' ' : z : zs
          | otherwise = z : zs
        (l:ls) = dropWhile isLower xs

layoutToText :: Layout -> Text
layoutToText l = Text.init $ foldLayout 0 (title l <> ": ") l

foldLayout :: Int -> Text -> Layout -> Text
foldLayout i t Layout {..} = bool concat empty check
  where go      = foldl' (foldLayout (succ i)) "" fields
        check   = Text.null go && Text.null result
        line    = "  |" <> Text.replicate i "  " <> title <> ": "
        empty   = bool t (t <> "Empty\n") $ i == 0
        content = bool line mempty $ i == 0
        concat  = t <> content <> result <> "\n" <> go

mkLogText :: Text -> [(Text, Text)] -> Text
mkLogText text lines = text
  <> Text.concat (mkLogTextLine <$> lines)

mkLogTextLine :: (Text, Text) -> Text
mkLogTextLine (key, value) = "\n  |  " <> key <> ": " <> value
