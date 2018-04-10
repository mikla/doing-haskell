module TypeClasses where

-- 7 :: Num a => a
-- Num - is a context

class Eqv a where
  (===) :: a -> a -> Bool
  (/==) :: a -> a -> Bool
  x /== y = not (x === y) -- default implementation

-- minimal complete definition

-- or
-- if functions have the same type it's allowed to write them as:
-- (==), (/=) :: a -> a -> Bool

instance Eqv Bool where
  True === True = True
  False === False = True
  _ === _ = False

  x /== y = not (x === y) -- you can override implementation


instance (Eqv a, Eqv b) => Eqv (a, b) where
  p1 === p2 = fst p1 === fst p2 && snd p1 === snd p2

-- 

class Printable a where
  toString :: a -> String

instance Printable Bool where
  toString True = "true"
  toString _ = "false"

instance Printable () where
  toString _ = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
  toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"