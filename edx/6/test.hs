replicate1 0 _ = []
replicate1 n x = x : replicate (n - 1) x

merge1 [] ys = ys 
merge1 xs [] = xs
merge1 (x : xs) (y : ys)
  = if x <= y then x : merge1 xs (y : ys) else y : merge1 (x : xs) ys

halve xs = splitAt (length xs `div` 2) xs

msort [] = []
msort [x] = [x]
msort xs = merge1 (msort ys) (msort zs)
  where (ys, zs) = halve xs

