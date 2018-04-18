module ListFunctions where

emptyList = []
someList = 3 : 5 : []
cons42 = (42 :)

--

addTwoElements x y l= x : y : l

nTimesLoop :: a -> Int -> [a] -> [a]
nTimesLoop a n acc = if n == 0 then acc else nTimesLoop a (n - 1) (a : acc)

nTimes :: a -> Int -> [a]
nTimes a n = nTimesLoop a n []

fst' ((,) x y) = x
fst'' ((x,y)) = x
fst''' ((x,_)) = x

head' ((:) x y) = x

tail' (x : xs) = xs
tail'' (_ : xs) = xs

second'' (_ : x : _) = x

sndHead = snd . head
sndHead' ((_, x) : _) = x

-- 3.1 (7th step)


oddsOnly :: Integral a => [a] -> [a]
oddsOnly x = loop x []
  where
    loop [] acc = reverse acc
    loop (x1:xs) acc = if x1 `mod` 2 /= 0 then loop xs (x1:acc) else loop xs acc


oddsOnly' :: Integral a => [a] -> [a]
oddsOnly' [] = []
oddsOnly' (x : xs)
  | odd x = x : oddsOnly' xs
  | otherwise = oddsOnly' xs


-- [x] -- single element list in pattern matching

sumTwo :: Num a => [a] -> [a] -> [a]
sumTwo l1 l2 = loop l1 l2 []
  where
    loop (x1:xs1) (x2:xs2) acc = loop xs1 xs2 ((x1 + x2) : acc)
    loop [] (x2:xs2) acc = loop [] xs2 (x2 : acc)
    loop (x1:xs1) [] acc = loop xs1 [] (x1 : acc)
    loop [] [] acc = reverse acc

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 l1 l2 l3 = sumTwo (sumTwo l1 l2) l3

groupElems :: Eq a => [a] -> [[a]]
groupElems x = loop x [] []
  where
    loop (x1 : xs1) [] acc = loop xs1 [x1] acc
    loop (x1:xs1) (g1:gs1) acc = if x1 == g1 then loop xs1 (x1 : g1 : gs1) acc else loop xs1 [x1] ((g1 : gs1) : acc)
    loop [] [] acc = reverse acc
    loop [] (x1:xs1) acc = reverse ((x1 : xs1) : acc)


-- !! get element by index

