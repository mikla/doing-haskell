module Identity where

import Control.Monad (ap, liftM)

newtype Identity a = Identity { runIdentity :: a } deriving Show

instance Functor Identity where
  fmap = liftM

instance Applicative Identity where
  pure = return
  (<*>) = ap

instance Monad Identity where
  return x = Identity x
--  Identity x >>= k = k x
  (>>=) (Identity x) f = f x

wrapnsucc :: Integer -> Identity Integer
wrapnsucc x = Identity (succ x)


chain = wrapnsucc 3 >>= wrapnsucc >>= wrapnsucc

-- Monad laws

-- return a >>= k === k a
-- m >>= return === m

goWrap0 =
  wrapnsucc 3 >>=
  wrapnsucc >>=
  wrapnsucc >>=
  return

goWrap1 =
  wrapnsucc 3 >>= (\x ->
  wrapnsucc x >>= (\y ->
  wrapnsucc y >>= (\z ->
  return z)))

goWrap2 =
  wrapnsucc 3 >>= (\x ->
  wrapnsucc x >>= (\y ->
  wrapnsucc y >>= (\z ->
  return (x, y, z))))

goWrap3 =
  wrapnsucc 3 >>= \x ->
  wrapnsucc x >>= \y ->
  wrapnsucc y >>
  return (x+y)

-- Do notation
-- do {e1 ; e2 } = e1 >> e2
-- do { p <- e1; e2} = e1 >>= \p -> e2
-- do { let v = e1; e2} = let v = e1 in do e2

goWrap4 =
  let i = 3 in
  wrapnsucc 3 >>= \x ->
  wrapnsucc x >>= \y ->
  wrapnsucc y >>
  return (i, x+y)

goWrap5 = do
  let i = 3
  x <- wrapnsucc i
  y <- wrapnsucc x
  wrapnsucc y
  return (i, x + y)



