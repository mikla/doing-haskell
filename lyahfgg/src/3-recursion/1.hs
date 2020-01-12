module RecMod where

max_ :: Ord a => [a] -> a
max_ [a] = a
max_ [] = error "empry.list"
max_ (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where
    maxTail = max_ xs

max__ :: Ord a => [a] -> a
max__ [a]    = a
max__ []     = error "error"
max__ (x:xs) = max x $ max__ xs

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x

-- todo
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _ = []

-- reverse
-- zip
-- elem
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) =
  let smaller = qsort [a | a <- xs, a <= x]
      bigger = qsort [a | a <- xs, a > x]
   in smaller ++ [x] ++ bigger

