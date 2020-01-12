module CompositionModule where

filtered = sum (filter (>10)(map (*2) [6, 2, 3]))

filtered' = sum $ filter (>10) $ map (*2) [6, 2, 3]

sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0