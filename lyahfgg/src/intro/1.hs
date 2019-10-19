module Intro where

-- What is Haskell?

-- Functional
-- Lazy
-- Statically typed


-- Loading modules
-- :l <your-file.hs>

-- Reloading
-- :r

-- Arithmetics

x = 5 + 4
y = 5 + (-3) -- <- Note () here

really = True || not False

-- Testing for equality

notEqual = 5 /= 3

-- Prefix

its10 = succ 9

its12 = max 12 9

-- Function application has highest priority

p1 = succ 9 + max 9 4 * 4 -- 46
p2 = succ (9) + (max 9 4) * 4

-- But

p3 = succ (9 * 2) + (max 9 4) * 4
-- Not
p4 = succ 9 * 2 + max 9 4 * 4

-- Infix

p5 = 10 `div` 2








