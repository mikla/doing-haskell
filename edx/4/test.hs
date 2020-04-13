import Data.Char

let2int c = ord c - ord 'a'
let2intU c = ord c - ord 'A'

int2let n = chr (ord 'a' + n)
int2letU n = chr (ord 'A' + n)

shift n c 
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | isUpper c = int2letU ((let2intU c + n) `mod` 26)
  | otherwise = c

encode n xs = [shift n x | x <- xs]

strToEncode  = "Think like a Fundamentalist Code like a Hacker"

excTest = encode 12 strToEncode