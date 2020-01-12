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

-- hh:mm:ssAM to 24h format
timeConversion :: String -> String
timeConversion s =
  let hh = take 2 s
      rest = take 6 $ drop 2 s
      hours = read hh :: Int
      hoursAdj
        | s !! 8 == 'P' = if hours == 12 then hours else hours + 12
        | (hours == 12) = 0
        | otherwise = hours
      hoursStr =
        if hoursAdj < 10
          then "0" ++ show hoursAdj
          else show hoursAdj
   in hoursStr ++ rest