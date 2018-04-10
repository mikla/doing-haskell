{-# LANGUAGE NoMonomorphismRestriction #-}

module Poly1 where 

import Data.Function

id :: t -> t
id x = x

getSecondFrom :: t0 -> t1 -> t2 -> t1
getSecondFrom a b c = b

dollar :: (a -> b) -> a -> b
dollar a b = a b

apply2 f x = f (f x)

flip2 f y x = f x y

-- 2.1

sumSquares = (+) `on` (^2)

g = (*)
h x = snd x
multSecond = g `on` h
res = multSecond ('A',2) ('E',7)

-- anon functions

res2 = (\x -> 2 + x - 7) 10
lenVect = \x -> \y -> 0

lenVect1 = \x y -> 0

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)