module LetModule where

squares =
  [ let square x = x * x
     in (square 4, square 5, square 6)
  ]

lets =
  let (a, b, c) = (1, 2, 3)
   in a + b + c

cased :: String -> Int
cased x =
  case x of
    "A" -> 1
    _ -> 0
