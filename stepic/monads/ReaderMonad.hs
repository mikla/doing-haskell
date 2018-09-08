module ReaderMonad where

import Control.Monad (ap, liftM)

{-
  instance Fuctor ((->) e) where
    fmap g h = g . h


-}

-- fmap :: Functor f => (a -> b) -> f a -> f b
-- length :: [a] -> Int
squareLen xs = fmap (^2) length xs

newtype Reader r a = Reader { runReader :: (r -> a) }

-- runReader :: Reader r a -> r -> a

instance Applicative (Reader r) where
  pure = return
  (<*>) = ap

instance Functor (Reader r) where
   fmap = liftM

instance Monad (Reader r) where
  return x = Reader $ \e -> x
  m >>= k = Reader $ \e ->
    let v = runReader m e
    in runReader (k v) e

ask :: Reader r r
ask = Reader id

asks :: (r -> a) -> Reader r a
asks = Reader

--

type User = String
type Password = String
type UsersTable = [(User, Password)]

pwds :: UsersTable
pwds = [("Bill", "123"), ("Ann", "qwerty")]

firstUser :: Reader UsersTable User
firstUser = do
  e <- ask
  return $ fst (head e)

firstUserPassword :: Reader UsersTable Password
firstUserPassword = do
    pwd <- asks (snd . head)
    return pwd

usersCount :: Reader UsersTable Int
usersCount = asks length

local :: (r -> r) -> Reader r a -> Reader r a
local f m = Reader $ \e -> runReader m (f e)

localTest :: Reader UsersTable (Int, Int)
localTest = do
  count1 <- usersCount
  count2 <- local (("Mike", "1"):) usersCount
  return (count1, count2)

reader :: (r -> a) -> Reader r a
reader f = do
  r <- ask
  return (f r)

local' :: (r -> r') -> Reader r' a -> Reader r a
local' f m = Reader $ \e -> runReader m (f e)

usersWithBadPasswords :: Reader UsersTable [Password]
usersWithBadPasswords = Reader $ \e -> map fst $ filter (\up -> (snd up == "123456")) e
