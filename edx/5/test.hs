fact1 n = product [1..n]

fact2 0 = 1
fact2 n = n * fact2 (n - 1)

length1 [] = 0
length1 (_:xs) = 1 + length1 xs

--reverse1 (x:xs) = reverse xs ++ x

zzip [] _ = []
zzip _ [] = []
zzip (x: xs) (y:ys) = (x, y) : zip xs ys