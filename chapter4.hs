-- HC4 - Haskell Chapter 4: Pattern Matching in Functions

-- Task 1: weatherReport
weatherReport :: String -> String
weatherReport "sunny"  = "It's a bright and beautiful day!"
weatherReport "rainy"  = "Don't forget your umbrella!"
weatherReport "cloudy" = "A bit gloomy, but no rain yet!"
weatherReport _        = "Weather unknown"

-- Task 2: dayType
dayType :: String -> String
dayType "Saturday" = "It's a weekend!"
dayType "Sunday"   = "It's a weekend!"
dayType day
  | day `elem` ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"] = "It's a weekday."
  | otherwise = "Invalid day"

-- Task 3: gradeComment
gradeComment :: Int -> String
gradeComment n
  | n >= 90 && n <= 100 = "Excellent!"
  | n >= 70 && n < 90   = "Good job!"
  | n >= 50 && n < 70   = "You passed."
  | n >= 0  && n < 50   = "Better luck next time."
  | otherwise           = "Invalid grade"

-- Task 4 & 5: specialBirthday
specialBirthday :: Int -> String
specialBirthday 1 = "Happy 1st Birthday!"
specialBirthday 18 = "Wow, you're 18 now!"
specialBirthday 21 = "Cheers to 21!"
specialBirthday age = "Happy Birthday! You are " ++ show age ++ " years old!"

-- Task 6: whatsInsideThisList
whatsInsideThisList :: [a] -> String
whatsInsideThisList [] = "The list is empty."
whatsInsideThisList [x] = "The list has one element."
whatsInsideThisList [x, y] = "The list has two elements."
whatsInsideThisList _ = "The list has many elements."

-- Task 7: firstAndThird
firstAndThird :: [a] -> [a]
firstAndThird (x:_:z:_) = [x, z]
firstAndThird _         = []

-- Task 8: describeTuple
describeTuple :: (String, Int) -> String
describeTuple (name, age) = name ++ " is " ++ show age ++ " years old."

-- Main function to test all tasks
main :: IO ()
main = do
  putStrLn "=== HC4 Haskell Chapter 4 Tasks ==="

  -- Task 1
  putStrLn "\nTask 1: weatherReport"
  print (weatherReport "sunny")
  print (weatherReport "cloudy")
  print (weatherReport "snowy")

  -- Task 2
  putStrLn "\nTask 2: dayType"
  print (dayType "Saturday")
  print (dayType "Monday")
  print (dayType "Funday")

  -- Task 3
  putStrLn "\nTask 3: gradeComment"
  print (gradeComment 95)
  print (gradeComment 75)
  print (gradeComment 55)
  print (gradeComment 10)
  print (gradeComment 110)

  -- Task 4 & 5
  putStrLn "\nTask 4 & 5: specialBirthday"
  print (specialBirthday 1)
  print (specialBirthday 18)
  print (specialBirthday 21)
  print (specialBirthday 30)

  -- Task 6
  putStrLn "\nTask 6: whatsInsideThisList"
  print (whatsInsideThisList ([] :: [Int]))
  print (whatsInsideThisList [1])
  print (whatsInsideThisList [1,2])
  print (whatsInsideThisList [1,2,3])

  -- Task 7
  putStrLn "\nTask 7: firstAndThird"
  print (firstAndThird [10, 20, 30, 40])
  print (firstAndThird [1])
  print (firstAndThird [1,2])

  -- Task 8
  putStrLn "\nTask 8: describeTuple"
  print (describeTuple ("Alice", 30))
  print (describeTuple ("Bob", 21))
