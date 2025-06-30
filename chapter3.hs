-- HC3 - Haskell Chapter 3 Practical Tasks

import Numeric (showHex)
import Text.Printf (printf)

-- Task 1: Check number sign using if-then-else
checkNumber :: Int -> String
checkNumber n = if n > 0 then "Positive"
                else if n < 0 then "Negative"
                else "Zero"

-- Task 2: Grading using guards
grade :: Int -> String
grade score
  | score >= 90 = "A"
  | score >= 80 = "B"
  | score >= 70 = "C"
  | score >= 60 = "D"
  | otherwise   = "F"

-- Task 3: RGB to Hex using let
rgbToHex :: (Int, Int, Int) -> String
rgbToHex (r, g, b) =
  let hex x = printf "%02X" x
  in hex r ++ hex g ++ hex b

-- Task 4: Heron's Formula
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c =
  let s = (a + b + c) / 2
  in sqrt (s * (s - a) * (s - b) * (s - c))

-- Task 5: Triangle Type
triangleType :: Float -> Float -> Float -> String
triangleType a b c
  | a == b && b == c = "Equilateral"
  | a == b || b == c || a == c = "Isosceles"
  | otherwise = "Scalene"

-- Task 6: Leap Year using if-then-else
isLeapYear :: Int -> Bool
isLeapYear year =
  if year `mod` 400 == 0 then True
  else if year `mod` 100 == 0 then False
  else if year `mod` 4 == 0 then True
  else False

-- Task 7: Season based on month
season :: Int -> String
season m
  | m == 12 || m == 1 || m == 2 = "Winter"
  | m >= 3 && m <= 5 = "Spring"
  | m >= 6 && m <= 8 = "Summer"
  | m >= 9 && m <= 11 = "Autumn"
  | otherwise = "Invalid Month"

-- Task 8: BMI Category using where
bmiCategory :: Float -> Float -> String
bmiCategory weight height
  | bmi < 18.5 = "Underweight"
  | bmi < 25   = "Normal"
  | bmi < 30   = "Overweight"
  | otherwise  = "Obese"
  where bmi = weight / (height ^ 2)

-- Task 9: Max of three using let
maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c =
  let maxAB = max a b
      maxABC = max maxAB c
  in maxABC

-- Task 10: Palindrome check using recursion
isPalindrome :: String -> Bool
isPalindrome s
  | length s <= 1 = True
  | head s == last s = isPalindrome (init (tail s))
  | otherwise = False

-- Main function to test all tasks
main :: IO ()
main = do
  putStrLn "=== HC3 Haskell Chapter 3 Tasks ==="

  -- Task 1
  putStrLn "\nTask 1: checkNumber"
  print (checkNumber 5)
  print (checkNumber (-3))
  print (checkNumber 0)

  -- Task 2
  putStrLn "\nTask 2: grade"
  print (grade 95)
  print (grade 72)
  print (grade 50)

  -- Task 3
  putStrLn "\nTask 3: rgbToHex"
  print (rgbToHex (255, 0, 127))
  print (rgbToHex (0, 255, 64))

  -- Task 4
  putStrLn "\nTask 4: triangleArea"
  print (triangleArea 3 4 5)
  print (triangleArea 7 8 9)

  -- Task 5
  putStrLn "\nTask 5: triangleType"
  print (triangleType 3 3 3)
  print (triangleType 5 5 8)
  print (triangleType 6 7 8)

  -- Task 6
  putStrLn "\nTask 6: isLeapYear"
  print (isLeapYear 2000)
  print (isLeapYear 1900)
  print (isLeapYear 2024)

  -- Task 7
  putStrLn "\nTask 7: season"
  print (season 3)
  print (season 7)
  print (season 11)

  -- Task 8
  putStrLn "\nTask 8: bmiCategory"
  print (bmiCategory 70 1.75)
  print (bmiCategory 90 1.8)

  -- Task 9
  putStrLn "\nTask 9: maxOfThree"
  print (maxOfThree 10 20 15)
  print (maxOfThree 5 25 10)

  -- Task 10
  putStrLn "\nTask 10: isPalindrome"
  print (isPalindrome "racecar")
  print (isPalindrome "haskell")
  print (isPalindrome "madam")
