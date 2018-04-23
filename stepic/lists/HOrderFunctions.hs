module HOrderFunctions where

import Prelude hiding (filter, takeWhile)
import Data.Char

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs)
  | p x = x : filter p xs
  | otherwise = filter p xs


takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p list@(x:xs) -- local alias
  | p x = x : takeWhile p xs
  | otherwise = []

l = filter (< 3) [1, 2, 3, 4]


readDigits :: String -> (String, String)
readDigits str = span isDigit str

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj _ _ [] = []
filterDisj f g (x:xs)
  | f x || g x = x : filterDisj f g xs
  | otherwise = filterDisj f g xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort left ++ [x] ++ qsort right
  where
    left = filter (<x) xs
    right = filter (>=x) xs


squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes xs = concat (map (\x -> [x^2, x^3]) xs)

-- perms :: [a] -> [[a]]

-- or, and, all, any
-- words, unwords

delAllUpper :: String -> String
delAllUpper str = unwords . filter (\x -> not (all isUpper x)) $ words str

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 [] [] [] = []
max3 (x:xs) (y:ys) (z:zs) = max z (max x y) : max3 xs ys zs


