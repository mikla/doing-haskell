module Recursion where 

factorial n = if n <= 0 then 1 else n * factorial (n - 2)

-- pattern matching

factorial' 0 = 1
factorial' n = if n < 0 then error "<0" else n * factorial' (n - 1)

-- error
-- undefined - is bottom type

factorial''' 0 = 1
factorial''' n | n < 0 = error "<0"
               | n > 0 = n * factorial''' (n - 1) 


factorial4 n | n > 0 = n * factorial''' (n - 1)               
             | otherwise = error "<0"


fibonacci (-1) = 1
fibonacci (-2) = -1
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n | n > 1 = fibonacci (n - 1) + fibonacci (n - 2)
            | n < -2 = fibonacci (n + 2) - fibonacci (n + 1)

-- fibonacci positive

fibonacciPos 0 = 0
fibonacciPos 1 = 1
fibonacciPos n | n > 0 = fibLoop 0 1 n
               | otherwise = error "<0"

fibLoop p1 p2 2 = p1 + p2
fibLoop p1 p2 n = fibLoop p2 (p1 + p2) (n - 1)

-- fibinacchi negative

fibonacciNeg (-1) = 1
fibonacciNeg (-2) = -1
fibonacciNeg n | n < 0 = fibLoop2 0 1 n
               | otherwise = error "<0"


fibLoop2 p1 p2 (-2) = p1 - p2
fibLoop2 p1 p2 n = fibLoop2 p2 (p1 - p2) (n + 1)
               
-- fibonachi all

fibinacchiAll 0 = 0
fibinacchiAll 1 = 1
fibinacchiAll (-1) = 1
fibinacchiAll (-2) = -1
fibinacchiAll n = fibLoopAll 0 1 n

fibLoopAll p1 p2 2 = p1 + p2
fibLoopAll p1 p2 (-2) = p1 - p2
fibLoopAll p1 p2 n | n > 0 = fibLoopAll p2 (p1 + p2) (n - 1)
                   | n < 0 = fibLoopAll p2 (p1 - p2) (n + 1)