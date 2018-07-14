module TypesWithParams where

import Data.Char(isDigit)
import Data.List(find)

-- type parameters

data Coord a = Coord a a

-- Coord (3::Int) (4::Int) :: Coord Int
-- :t Coord
-- Coord :: a -> a -> Coord a

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) = sqrt((x1 - x2)^2 + (y1 - y2)^2)

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = abs(x1 - x2) + abs(y1 - y2)

twice :: a -> [] a -- same as [a]
twice x = [x, x]

thrice :: a -> (,,) a a a
thrice a = (,,) a a a

findDigit :: [Char] -> Maybe Char
-- findDigit x = (find isDigit) x
findDigit = foldr (\b a -> if isDigit b then Just b else a) Nothing

findDigitOrX :: [Char] -> Char
findDigitOrX x = case findDigit x of
  Just x -> x
  Nothing -> 'X'

maybeToList :: Maybe a -> [a]
maybeToList m = case m of
  Just x -> x : []
  Nothing -> []

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x: xs) = Just x

--

data Error = ParsingError | IncompleteDataError | IncorrectDataError String
data StrEither = Either String

data CoordLazy a = CoordLazy a a deriving Show
data CoordStrict a = CoordStrict !a !a deriving Show
getXLazy :: CoordLazy a -> a
getXStrict :: CoordStrict a -> a
getXLazy (CoordLazy a _) = a
getXStrict (CoordStrict a _) = a