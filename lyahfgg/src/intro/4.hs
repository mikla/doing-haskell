module IntroListComp where

listComp = [x * 2 | x <- [1..10]]
listCompCond = [x * 2 | x <- [1..10], x * 2 > 100]

customLength xs = sum [1 | _ <- xs]


-- Tuples
-- fst
-- snd
rightTriangles = [(a, b, c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]

-- :: Bool
-- Putting type to the expression

-- [Bool] -> [Bool] that's how function type look ->