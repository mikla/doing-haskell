module ClassExtensions where

import TypeClasses

-- class extension

class (Eqv a) => Ordv a where
  (<), (<=), (>=), (>) :: a -> a -> Bool
  max, min :: a -> a -> a
  compare :: a -> a -> Ordering -- LT | EQ | GT

-- multiple extensions

class (Eqv a, Printable a) => MyClass a where


-- 2.4

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a | doesEnrageMork a && doesEnrageGork a = stomp (stab a)
                  | doesEnrageMork a = stomp a
                  | doesEnrageGork a = stab a
                  | otherwise = a

-- Show & Read

-- 2.4
a = 127.2
b = 24.1
c = 20.1
d = 2

ip = show a ++ show b ++ show c

-- minBound, maxBound

-- fromEnum :: a -> Int
-- toEnum :: Int -> a

class (Enum a, Bounded a, Eq a) => SafeEnum a where
  ssucc :: a -> a

  ssucc a | a == maxBound = minBound
          | otherwise = succ a

  spred :: a -> a
  spred a | a == minBound = maxBound
          | otherwise = pred a

instance SafeEnum Bool

-- standard type classes

-- Num
-- Integral
-- Fractional
-- Floating
-- RealFloat


avg :: Int -> Int -> Int -> Double
avg x y z = fromIntegral x / 3 + fromIntegral y / 3 + fromIntegral z / 3
