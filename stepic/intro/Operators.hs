module Demo where

f0 = 6 `max` 7 -- call function in operator style
f2 = (+) 6 7 -- call operator in functional style

f3 = 3 + 5 * 8

-- substraction left associative

-- infixl, infixr [priority]

-- haskell doesn't have predefined operators
-- all operators deined in standard library

infixl 6 *+*

a *+* b = a ^ 2 + b ^ 2

-- prefix style

infixl 6 *|* 
(*|*) a b = a ^ 2 + b ^ 2

infixl 6 |-|
(|-|) a b = abs(a - b)


--- f $ x = f x

-- $ has 0 priority
-- this means that we can use it like this:
-- sin $ pi / 2 (without putting pi/2 to braces)
-- $ right associatove

-- rewrite logBase 4 (min 20 (9 + 7)) with $
-- logBase 4 $ min 20 $ 9 + 7

