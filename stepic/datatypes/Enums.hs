module Enums where

data Bool1 = True | False

data B = T | F deriving (Show, Eq, Read, Enum)

not' :: B -> B
not' T = F
not' F = T

data Color = Red | Green | Blue

instance Show Color where
  show Red = "Red"
  show Blue = "Blue"
  show Green = "Green"

charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9

data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering

cmp Error Warning = GT
cmp Error Info = GT
cmp Warning Error = LT
cmp Warning Info = GT
cmp Info Warning = LT
cmp Info Error = LT
cmp Warning Warning = EQ
cmp Error Error = EQ
cmp Info Info = EQ


-- lessThanError lvl =
--  case cmp lvl Error of
--    LT -> True
--    _ -> False

data Result = Fail | Success

doSomeWork :: SomeData -> (Result, Int)
doSomeWork _ = (Success, 1)

processData :: SomeData -> String
processData d = case doSomeWork d of
  (Success, _) -> "Success"
  (Fail, n) -> "Fail: " + show n


