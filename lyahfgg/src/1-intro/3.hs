module IntroToLists where

l = [1, 2, 3]

-- l :: [Integer]

-- [[Integer]] list of lists

-- :t "what?|
-- [Char]

-- All functions from list apeals to Char

-- Strings are just syntactyc sugar for [Char]

l2 = 5 : [2, 3] -- similar like in Scala

str1 = 'a' : "lolo"

-- !! getting element by index

index2 = l !! 2


-- you can compare lists

whatIsMore = [2, 1, 0] > [3, 2, 0]

-- null is a function that checks if list is empty

isEmpty l = null l


-- take
-- drop
-- minimum
-- maximum
-- sum
-- product

-- elem takes a thing and a list of things and tells us if that thing is an element of the list.

isInside = 4 `elem` [3, 4, 5]


ranges = [1..20]

allEven = [2, 4..52]

lazyList = [13, 26..]
lazyList16 = take 16 lazyList

-- cycle takes a list and cycles it into an infinite list.

-- repeat