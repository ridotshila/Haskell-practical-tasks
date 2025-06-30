-- HC5 - Haskell Chapter 5: Improving and Combining Functions

import Data.Char (isUpper)

-- Task 1: Apply a function three times
applyThrice :: (Int -> Int) -> Int -> Int
applyThrice f x = f (f (f x))

-- Task 2: Filter odd numbers from 1 to 30
oddNumbers :: [Int]
oddNumbers = filter odd [1..30]

-- Task 3: Check if any word starts with an uppercase letter
hasUppercaseStart :: [String] -> Bool
hasUppercaseStart = any (\word -> not (null word) && isUpper (head word))

-- Task 4: Lambda function
biggerThan10 :: Int -> Bool
biggerThan10 = \x -> x > 10

-- Task 5: Partial application
multiplyByFive :: Int -> Int
multiplyByFive = (*) 5

-- Task 6: Function composition to square numbers and filter even results
evenSquares :: [Int] -> [Int]
evenSquares = filter even . map (^2)

-- Task 7: Using the $ operator
result :: Int
result = sum $ map (*2) $ filter (>3) [1..10]

-- Task 8: Point-free style
addFive :: Int -> Int
addFive = (+5)

-- Task 9: Transform list using a function applied twice
transformList :: (a -> a) -> [a] -> [a]
transformList f = map (f . f)

-- Task 10: Combine filter, map, and any to check for squared values > 50
anySquareGreaterThan50 :: [Int] -> Bool
anySquareGreaterThan50 = any (>50) . map (^2)

-- Main to demonstrate all functions
main :: IO ()
main = do
  putStrLn "=== HC5 Haskell Chapter 5 Tasks ==="

  -- Task 1
  putStrLn "\nTask 1: applyThrice (+1) 4"
  print (applyThrice (+1) 4)  -- Should be 7

  -- Task 2
  putStrLn "\nTask 2: odd numbers from 1 to 30"
  print oddNumbers

  -- Task 3
  putStrLn "\nTask 3: hasUppercaseStart [\"hello\", \"World\"]"
  print (hasUppercaseStart ["hello", "World"])  -- True

  -- Task 4
  putStrLn "\nTask 4: biggerThan10 12"
  print (biggerThan10 12)

  -- Task 5
  putStrLn "\nTask 5: multiplyByFive 6"
  print (multiplyByFive 6)

  -- Task 6
  putStrLn "\nTask 6: evenSquares [1..10]"
  print (evenSquares [1..10])

  -- Task 7
  putStrLn "\nTask 7: result = sum $ map (*2) $ filter (>3) [1..10]"
  print result

  -- Task 8
  putStrLn "\nTask 8: addFive 10"
  print (addFive 10)

  -- Task 9
  putStrLn "\nTask 9: transformList (+1) [1,2,3]"
  print (transformList (+1) [1,2,3])  -- [3,4,5]

  -- Task 10
  putStrLn "\nTask 10: anySquareGreaterThan50 [3,5,8]"
  print (anySquareGreaterThan50 [3,5,8])  -- True
