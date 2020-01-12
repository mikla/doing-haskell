module FunctionApplication where

-- $ function application

{-
  Function application is left-associative
  f a b c === (((f a) b) c)
  But!
  $ - is right-associative

  ($) :: (a -> b) -> a -> b
  f $ x = f x

-}

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g = \x -> f (g x)

negated = map (\x -> negate (abs x)) [5, -3]
negated' = map(negate . abs) [5, -3]

{-
  . (function composition) is right assotiative
  so, each time you see this :
  f (g (z x)) equivalent to f . g . z
-}

n2 = map (\x -> negate (sum (tail x))) [[1], [-3], [4]]
n3 = map (negate . sum . tail) [[1], [-3], [4]]

v2 = sum (replicate 5 (max 6.7 8.9))
v3 = (sum . replicate 5 . max 6.7) 8.9
v4 = sum . replicate 5 . max 5 $ 2

-- However, reading composition chain may be sometimes difficult.

oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<1000) .filter odd . map (^2) $ [1..]

oddSquareSum_ :: Integer
oddSquareSum_ =
  let
    oddSquares = filter odd $ map(^2) [1..]
    belowLimit = takeWhile(<1000) oddSquares
  in sum belowLimit