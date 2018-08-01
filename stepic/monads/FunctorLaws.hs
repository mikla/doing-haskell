module FunctorLaws where

import Prelude hiding (Functor, fmap)

class Functor f where
  fmap :: (a -> b) -> f a -> f b

data Point3D a = Point3D a a a deriving Show

instance Functor Point3D where
  fmap f (Point3D a b c) = Point3D (f a) (f b) (f c)


data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)

instance Functor GeomPrimitive where
  fmap f (Point p) = Point $ fmap f p
  fmap f (LineSegment a b) = LineSegment (fmap f a) (fmap f b)

-- <$> map


instance Functor ((->) e) where
  fmap = (.)

  -- (a -> b) -> ((->) e a) -> ((->) e b
  -- (a -> b) -> (e -> a) -> e -> b

data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

instance Functor (Entry k1 k2) where
  fmap f (Entry (k1, k2) v) = Entry (k1, k2) (f v)

instance Functor (Map k1 k2) where
  fmap f (Map entries) = Map (map (fmap f) entries)

-- Functor Laws

-- fmap id
-- fmap (f . g) = fmap f . fmap g