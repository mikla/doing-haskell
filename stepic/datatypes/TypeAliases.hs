module TypeAliases where

import Prelude hiding(mempty, mappend, Monoid)

type Str = [Char] -- Just alias

newtype IntList = IList [Int] deriving Show -- One constructor and Lazy
example = IList [1, 2]

class Monoid a where
  mempty :: a
  mappend :: a -> a -> a

  mconcat :: [a] -> a
  mconcat = foldr mappend mempty

instance Monoid [a] where
  mempty = []
  mappend = (++)

newtype Sum a = Sum {getSum :: a}
  deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  Sum x `mappend` Sum y = Sum (x + y)

newtype Product a = Product {getProduct :: a}
  deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Product a) where
  mempty = Product 1
  Product x `mappend` Product y = Product(x * y)

newtype Xor = Xor {getXor :: Bool} deriving (Eq, Show)

instance Monoid Xor where
  mempty = Xor False
  Xor x `mappend` Xor y = Xor (x /= y)

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)
  (x1, y1) `mappend` (x2, y2) =
    (x1 `mappend` x2, y1 `mappend` y2)


newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

--instance Monoid a => Monoid (Maybe' a) where
--  mempty = Maybe' $ Just mempty
--  mappend (Maybe' Nothing) _ = Maybe' Nothing
--  mappend _ (Maybe' Nothing) = Maybe' Nothing
--  mappend (Maybe' a) (Maybe' b) = Maybe' (mappend a b)

newtype Endo a = Endo {getEndo :: a -> a}
instance Monoid (Endo a) where
  mempty = Endo id
  Endo a `mappend` Endo b = Endo (a . b)
