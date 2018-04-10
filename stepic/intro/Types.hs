module Types where

import Data.Char

-- Bool: True, False
-- Float, Double

x = 3 :: Int
y = 3 :: Double
z = y + 17 -- Double as well

discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

standardDiscount :: Double -> Double
standardDiscount = discount 1000 5

is7Disgit = isDigit '7'

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y then digitToInt x * 10 + digitToInt y else 100 

-- Кортеж
-- Not signle element cartges
-- () empty

--         x1      y1          x2      y2
dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt ((fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2)

-- [1, 2, 3] list
-- ['H', 'i'] :: [Char]
-- "Hi" :: [Char]
-- "Hi" :: String

-- : add to head
str = 'c' : "jel"

-- ++
str1 = str ++ ['c', 'j']

