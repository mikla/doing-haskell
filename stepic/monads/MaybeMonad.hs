module MaybeMonad where

import Data.Char(isDigit)

type Name = String
type DataBase = [(Name, Name)]

fathers, mothers :: DataBase
fathers = [("Bill", "John")]
mothers = [("Bill", "Jane")]

getM, getF :: Name -> Maybe Name
getM person = lookup person mothers
getF person = lookup person fathers

billFathers = do {f <- getF "Bill"; gm <- getM f; getM gm}


data Token = Number Int | Plus | Minus | LeftBrace | RightBrace deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken number = if (all isDigit number) then Just $ Number $ read number else Nothing

tokenize :: String -> Maybe [Token]
-- tokenize str = sequence $ map asToken $ words str
tokenize str = mapM asToken $ words str


list = [(x, y) | x <- [1, 2, 3], y <- [1, 2, 3], x /= y]

pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x = [(a, b, c) | a <- [1..x], b <- [1..x], c <- [1..x], a ^ 2 + b ^ 2 == c ^ 2, a < b]
