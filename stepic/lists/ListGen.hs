module ListGen where

import Data.List

-- infinite program
bot = not bot

-- infinite list
ones :: [Integer]
ones = 1 : ones

nats n = n : nats (n + 1)

nats10 = take 10 $ nats 5

squares = map (^2) $ nats 1

-- infinite fibonachi numbers

fs x y = (x + y) : (fs y (x + y))

fibStream :: [Integer]
fibStream = zipWith (+) (fs 0 1) (fs 1 1)

-- repeat
-- replicate
-- cycle [a] -> [a] repeats list

-- take 2 $ iterate (^2) 2
-- [2,4,16,256,65536]

repeat' :: a -> [a]
repeat' = iterate id

-- 3.3

-- [1..10]
-- enumFromTo
-- ['a'..'z']

-- [1, 3..10]
-- enumFromThenTo

-- [1..]
-- enumFrom

data Odd = Odd Integer deriving (Eq, Show)

oddX = Odd 33

instance Enum Odd where
  toEnum x = Odd (toInteger x)
  fromEnum (Odd x) = fromIntegral x
  succ (Odd x) = Odd (x + 2)
  pred (Odd x) = Odd (x - 2)
  enumFrom = iterate succ
  enumFromTo (Odd x) (Odd y) = map Odd [x, x + 2 .. y]

test0 = succ (Odd 1) == (Odd 3)
test1 = pred (Odd 3) == (Odd 1)
-- enumFrom
test2 = (take 3 $ [Odd 1 ..]) == [Odd 1,Odd 3,Odd 5]
-- enumFromTo
-- -- По возрастанию
test3 = (take 3 $ [Odd 1..Odd 7]) == [Odd 1,Odd 3,Odd 5]
-- -- По убыванию
test4 = (take 3 $ [Odd 7..Odd 1]) == []
-- enumFromThen
-- -- По возрастанию
test5 = (take 3 $ [Odd 1, Odd 3 ..]) == [Odd 1,Odd 3,Odd 5]
-- -- По убыванию
test6 = (take 3 $ [Odd 3, Odd 1 ..]) == [Odd 3,Odd 1,Odd (-1)]
-- enumFromThenTo
-- -- По возрастанию
test7 =([Odd 1, Odd 5 .. Odd 7]) == [Odd 1,Odd 5]
test7_1=([Odd 1, Odd 3 .. Odd 7]) == [Odd 1,Odd 3,Odd 5,Odd 7]

-- -- По убыванию
test8 =([Odd 7, Odd 5 .. Odd 1]) == [Odd 7,Odd 5,Odd 3,Odd 1]
-- -- x1 < x3 && x1 > x2
test9 =([Odd 7, Odd 5 .. Odd 11]) == []
-- -- x1 > x3 && x1 < x2
test10 =([Odd 3, Odd 5 .. Odd 1]) == []

allTests = zip [0..] [test0, test1, test2, test3, test4, test5, test6, test7, test7_1, test8, test9, test10]

-- list comprehension

xsList = [1..20]
squares2 = [x^2 | x <- xsList, x^2 < 200]

pairs = [(x, y) | x <- [1, 2], y <- [1, 2]]

pifagorTriplets = [(x, y, z) | x <- xsList, y <- xsList, z <- xsList, x^2 + y^2 == z^2, x<= y]

-- change

coins :: (Ord a, Num a) => [a]
coins = [2, 3, 7]

change :: (Ord a, Num a) => a -> [[a]]
change 0 = [[]]
change s = [coin:ch | coin <- coins, coin <= s, ch <- (change $ s - coin)]

meanList :: [Double] -> Double
someFun :: [Double] -> Double
someFoldingFun :: Double -> [Double] -> [Double]
someInit :: [Double]

someFun (x : xs) = x / fromIntegral (length xs)
someFoldingFun x acc = (x + head acc) : acc
someInit = [0]
meanList = someFun .  foldr someFoldingFun someInit


evenOnly :: [a] -> [a]
evenOnly xs = reverse $ foldl foldF [] (zip xs [1..]) where
  foldF :: [a] -> (a, Integer) -> [a]
  foldF acc (x, index) | even index = x : acc
                       | otherwise = acc

evenOnly' :: [a] -> [a]
evenOnly' s = evenOnlyLoop (zip s [1..]) where
  evenOnlyLoop :: [(a, Integer)] -> [a]
  evenOnlyLoop [] = []
  evenOnlyLoop ((x, index) : xs) | even index = x : evenOnlyLoop xs
                                 | otherwise = evenOnlyLoop xs

lastElem :: [a] -> a
lastElem = foldl1 (\a b -> b)

-- unfold' :: (b -> (a,b)) -> b -> [a]
-- unfold' f ini = let (x, ini') = f ini

revRange :: (Char,Char) -> [Char]
revRange (a, b) = unfoldr g b where
    g s = if (s >= a && s <= b) then Just (s, pred s) else Nothing
