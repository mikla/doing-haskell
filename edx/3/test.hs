--import Prelude hiding ((||))
import Prelude hiding ((&&))

abs1 n = if n > 0 then n else -n

abs2 n | n > 0 = n
	   | otherwise = -n

signum n | n < 0 = -1
  | n == 0 = 0
  | otherwise = 1

const1 x = \_ -> x

halve1 xs = (take n xs, drop n xs)
  where n = length xs `div` 2

halve2 xs = splitAt (div (length xs) 2) xs

halve3 xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
  where n = length xs

-- 2
safetail1 xs = if null xs then [] else tail xs

safetail2 xs
  | null xs = []
  | otherwise = tail xs 


safetail3 [] = []
safetail3 xs = tail xs

safetail4 = 
	\ xs -> case xs of 
		[] -> []
		(_ : xs) -> xs


-- 3

{-| 

False || False = False
_ || _ = True
--
False || b = b
True || _ = True
--
b || c 
    | b == c = c
    | otherwise = True
--
b || False = b
_ || True = True
--
False || False = False
False || True = True
True || False = True
True || True = True

-}

-- 4 

-- a && b = if a then if b then True else False else False
-- a && b = if b then a else False 

mult = \ x -> (\ y -> (\ z -> x * y * z))

rem1 n xs =  take n xs ++ drop (n + 1) xs

funct x xs = take (x + 1) xs ++ drop x xs