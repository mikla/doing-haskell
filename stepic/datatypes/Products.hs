module Products where

import Enums

data Point = Point Double Double deriving Show

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt(x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)


data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = pi * r ^ 2
area (Rectangle x y) = x * y


data Result' = Fail' Int | Success'

instance Show Result' where
    show (Fail' x) = "Fail: " ++ show x
    show Success' = "Success"

doSomeWork' :: SomeData -> Result'
doSomeWork' x = case doSomeWork x of
   (Fail, code) -> Fail' code
   (Success, _) -> Success'

isSquare :: Shape -> Bool
isSquare (Circle _) = False
isSquare (Rectangle x y) | x == y = True
                         | otherwise = False

-- Lazy pattern matching
(***) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
-- (***) f g p = (f $ fst p, g $ snd p)
-- (***) f g (x, y) = (f x, g y) -- will fail if we pass undefined as pair
(***) f g ~(x, y) = (f x, g y)


