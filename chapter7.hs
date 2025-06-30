-- HC7 - Haskell Chapter 7: Intro to Type Classes

import Text.Read (readMaybe)

-- Task 1 & 2: Define Color with Eq and Ord
data Color = Red | Green | Blue
  deriving (Show, Read, Bounded, Enum)

instance Eq Color where
  Red == Red     = True
  Green == Green = True
  Blue == Blue   = True
  _ == _         = False

instance Ord Color where
  compare Red Red     = EQ
  compare Red _       = LT
  compare Green Red   = GT
  compare Green Green = EQ
  compare Green Blue  = LT
  compare Blue Blue   = EQ
  compare Blue _      = GT

-- Task 3: Function with Eq and Ord
compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues a b = if a >= b then a else b

-- Task 4: Define Shape with Show, Read, Eq, Ord
data Shape = Circle Double | Rectangle Double Double
  deriving (Eq)

instance Show Shape where
  show (Circle r) = "Circle " ++ show r
  show (Rectangle w h) = "Rectangle " ++ show w ++ " " ++ show h

instance Read Shape where
  readsPrec _ input =
    case words input of
      ("Circle":r:_)       -> [(Circle (read r), "")]
      ("Rectangle":w:h:_)  -> [(Rectangle (read w) (read h), "")]
      _                    -> []

-- Added Ord instance based on area
instance Ord Shape where
  compare s1 s2 = compare (area s1) (area s2)
    where
      area (Circle r)      = pi * r * r
      area (Rectangle w h) = w * h

-- Task 5: Function with Num constraint
squareArea :: Num a => a -> a
squareArea side = side * side

-- Task 6: Integral and Floating
circleCircumference :: (Floating a, Integral b) => b -> a
circleCircumference r = 2 * pi * fromIntegral r

-- Task 7: Bounded and Enum
nextColor :: Color -> Color
nextColor color =
  if color == maxBound then minBound else succ color

-- Task 8: Read to parse Shape
parseShape :: String -> Maybe Shape
parseShape s = readMaybe s :: Maybe Shape

-- Task 9: Describable type class
class Describable a where
  describe :: a -> String

instance Describable Bool where
  describe True  = "This is true."
  describe False = "This is false."

instance Describable Shape where
  describe (Circle r) = "A circle with radius " ++ show r
  describe (Rectangle w h) = "A rectangle of width " ++ show w ++ " and height " ++ show h

-- Task 10: describeAndCompare
describeAndCompare :: (Describable a, Ord a) => a -> a -> String
describeAndCompare a b = describe (compareValues a b)

-- Main function
main :: IO ()
main = do
  putStrLn "=== HC7 Haskell Chapter 7 Tasks ==="

  -- Task 1 & 2
  putStrLn "\nTask 1 & 2: Color Eq and Ord"
  print (Red == Green)
  print (Red < Green)
  print (Blue > Red)

  -- Task 3
  putStrLn "\nTask 3: compareValues 5 8"
  print (compareValues 5 8)

  -- Task 4
  putStrLn "\nTask 4: Show and Read Shape"
  let s1 = Circle 3.5
  let s2 = read "Rectangle 4.0 5.0" :: Shape
  print s1
  print s2

  -- Task 5
  putStrLn "\nTask 5: squareArea 6"
  print (squareArea 6)

  -- Task 6
  putStrLn "\nTask 6: circleCircumference 7"
  print (circleCircumference 7 :: Double)

  -- Task 7
  putStrLn "\nTask 7: nextColor Red, nextColor Blue"
  print (nextColor Red)
  print (nextColor Blue)

  -- Task 8
  putStrLn "\nTask 8: parseShape \"Circle 5.0\""
  print (parseShape "Circle 5.0")
  print (parseShape "Invalid")

  -- Task 9
  putStrLn "\nTask 9: describe Bool and Shape"
  print (describe True)
  print (describe (Rectangle 2.0 3.0))

  -- Task 10
  putStrLn "\nTask 10: describeAndCompare Circle 3.0 vs Circle 5.0"
  print (describeAndCompare (Circle 3.0) (Circle 5.0))
