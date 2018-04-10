module LocalC where 

roots :: Double 
      -> Double
      -> Double
      -> (Double, Double)

roots a b c =
  let d = sqrt (b ^ 2 - 4 * a * c) in
  ((-b -d) / (2 * a), (-b + d) / (2 * a))      


roots2 a b c =
  let 
    aTwice = 2 * a
    x1 = (-b - d) / aTwice
    x2 = (-b + d) / aTwice
    d = sqrt $ b ^ 2 - 4 * a * c
  in  (x1, x2)

rootsDiff a b c = let
  (x1, x2) = roots a b c
  in x2 - x1

seqA :: Integer -> Integer
seqA 0 = 1
seqA 1 = 2
seqA 2 = 3
seqA k = 
  let 
    loop p1 p2 p3 2 = p3
    loop p1 p2 p3 n = 
      let new = p3 + p2 - 2 * p1
      in loop p2 p3 new (n - 1)
  in loop 1 2 3 k


  -- where

roots3 a b c = (x1, x2) where
  aTwice = 2 * a
  x1 = (-b - d) / aTwice
  x2 = (-b + d) / aTwice
  d = sqrt $ b ^ 2 - 4 * a * c


-- Diff: 
-- Let ... in is an expression (you can assing it to variable)
-- Where 


factorial7 :: Integer -> Integer
factorial7 n | n >=0 = helper 1 n     -- <-- and here 
             | otherwise = error "<0" -- <-- helper available here as well 
  where
    helper acc 0 = acc
    helper acc n = helper (acc * n) (n-1)

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count 0 = (0, 1)
sum'n'count x = (sum1, toInteger count1) where
  digs :: Integer -> [Integer]
  digs 0 = []
  digs x = digs (x `div` 10) ++ [x `mod` 10]
  digsList = digs (abs x)
  sum1 = sum digsList
  count1 = length digsList

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = res where
  n = 1000 :: Double
  h = (b - a) / n
  loop :: Double -> Double -> Double -> Double
  loop acc xi nx | nx > n - 1 = acc
                 | otherwise = loop (acc + (f xi)) (xi + h) (n + 1)
  res = loop 0 a 1
  -- res = h * (((((f a) + (f b))) / 2) + (loop 0 (a + h) 1))