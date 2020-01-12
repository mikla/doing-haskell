module PatternMatching where

lucky :: (Integral a) => a -> String
lucky 7 = "SEVEN"
lucky _ = "Sorry, No luck"

fuctorial :: (Integral a) => a -> a
fuctorial 0 = 1
fuctorial n = n * fuctorial (n - 1)

-- pattern matching is quite important in tuples
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- pattern matching lists
-- head : tail, similar like in Scala
head' :: [a] -> Maybe a
head' (a:_) = Just a -- extracting whole list all@(x:xs)
head' _ = Nothing

