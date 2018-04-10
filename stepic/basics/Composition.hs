module Composition where 

import Data.Tuple

-- 2.2 Function composiotion

doItYourself = f . g . h

f = (logBase 2)
g = (^3)
h = max 42

pair = (,) True 2 -- (True, 3) prefix style
tripple = (,,) True 3 's'

swap = f (g h)
