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