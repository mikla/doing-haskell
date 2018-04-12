module NonStrictSemantics where

sumIt :: Int -> Int -> Int
sumIt x y = x + y

-- lazy evaluation

dup :: Int -> (Int, Int)
dup x = (x, x)

{-
  dup (2+3)
  ~> (p, p)   p = 5 (evaluates only once)
  ~> (5, p)
  ~> (5, 5)
-}

bar x y z = x + y
foo a b = bar a a (a + b)
value = foo (3 * 10) (5 - 2)

{-
  foo (3 * 10) (5 - 2)            ignored
  ~> bar (3 * 10) (3 * 10) (3 * 10 + 5 - 2) | 3 * 10 = p
  ~> (3 * 10) + (3 * 10)

  bar p p
-}


const42 :: a -> Int
const42 = const 42


{-
 строгие и нестрогие функции
-}

{-
  NF (Normal form)
  42
  (3,4)
  \x -> x + 2

-}

{-
 not NF
 "Real" ++ "world"
 sin (pi / 2)
 (\x -> x + 2) 5
 (3, 1 + 5)

-}

-- Weak head normal form
{-
  \x -> x + 2 * 3
  (3, 1 + 5)
  (, 4 * 5)
  (+) (7^2)
-}


{-
  seq:: a -> b -> b
-}

-- $!
-- x - будет приведен к weak normal form
-- f $! x = x `seq` f x

-- const 42 $! undefined