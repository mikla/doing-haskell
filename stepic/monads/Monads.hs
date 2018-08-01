module Monads where

import Control.Monad (ap, liftM)

-- a -> m b - стрелка Клейсли

data Log a = Log [String] a deriving Show

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f message a = Log [message] (f a)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers a toLogB toLogC = Log [m1, m2] c where
  Log [m1] b = toLogB a
  Log [m2] c = toLogC b

toKleisli :: Monad m => (a -> b) -> (a -> m b)
toKleisli f = \x -> return (f x)


add1Log = toLogger (+1) "added one"
mult2Log = toLogger (* 2) "multiplied by 2"

returnLog :: a -> Log a
returnLog a = Log [] a

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log m1 a) f = Log (append m1 m2) c where
  append xs ys = foldr (:) ys xs
  Log m2 c = f a

instance Functor Log where
  fmap = liftM

instance Applicative Log where
  pure = return
  (<*>) = ap

instance Monad Log where
  return = returnLog
  (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList a list = foldl (\log f -> bindLog log f) (return a) list
