module Guards where

bmi :: (RealFloat a) => a -> String
bmi b
  | b <= 18.5 = "Underweight"
  | b <= 25.0 = "Normal"
  | otherwise = "...."

-- Inline
max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

-- Where
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell w h
  | bmi <= 18.5 = "<18.5"
  | bmi <= 20 = "<20"
  | otherwise = "other"
  where
    bmi = h / w
