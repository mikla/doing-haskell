module Functions where
f = acos (cos pi)
m = max 5 42
m2 = (max 5) 42 -- assotiave to left, partial application

lb1 = logBase 2 8
lb2 = (logBase 2) 8

-- function definitions
-- function names and paramters must be lower case
sumSquares x y = x^2 + y^2

rock'n'roll = 42

-- defining function in ghci with let

lenVec2 x y z = sqrt(x^2 + y^2 + z^2)

-- every function that doesn't accept and argument - constant.
fortyTwo = 39 + 3

-- all if and else branches must be present!
conditionanl x = (if x > 0 then 1 else (-1)) + 3

sign x = if x > 0 then 1 else if (x < 0) then (-1) else 0

-- partial application
max5 x = max 5 x
max5' = max 5

discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum
-- partially applied
standardDiscount = discount 1000 5

translate languageTo languageFrom text = text

translateFromSpanishToRussian = translateToRussian "es"
translateFromEnglishToRussian = translateToRussian "en"
translateToRussian = translate "ru"