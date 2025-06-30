-- HC6 - Haskell Chapter 6: Recursion and Folds

import Data.Char (digitToInt)

-- Task 1: Factorial using recursion
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Task 2: Fibonacci using recursion
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- Task 3: Sum using foldr
sumList :: [Int] -> Int
sumList = foldr (+) 0

-- Task 4: Product using foldl
productList :: [Int] -> Int
productList = foldl (*) 1

-- Task 5: Reverse using recursion
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- Task 6: Element exists
elementExists :: Eq a => a -> [a] -> Bool
elementExists _ [] = False
elementExists x (y:ys)
  | x == y    = True
  | otherwise = elementExists x ys

-- Task 7: Length of list
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

-- Task 8: Filter even numbers
filterEvens :: [Int] -> [Int]
filterEvens [] = []
filterEvens (x:xs)
  | even x    = x : filterEvens xs
  | otherwise = filterEvens xs

-- Task 9: Map function
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

-- Task 10: Digits of a number
digits :: Int -> [Int]
digits n
  | n < 10    = [n]
  | otherwise = digits (n `div` 10) ++ [n `mod` 10]

-- Main function
main :: IO ()
main = do
  putStrLn "=== HC6 Haskell Chapter 6 Tasks ==="

  -- Task 1
  putStrLn "\nTask 1: Factorial of 5"
  print (factorial 5)

  -- Task 2
  putStrLn "\nTask 2: 7th Fibonacci number"
  print (fibonacci 7)

  -- Task 3
  putStrLn "\nTask 3: Sum of [1..5]"
  print (sumList [1..5])

  -- Task 4
  putStrLn "\nTask 4: Product of [1..5]"
  print (productList [1..5])

  -- Task 5
  putStrLn "\nTask 5: Reverse [1,2,3,4]"
  print (reverseList [1,2,3,4])

  -- Task 6
  putStrLn "\nTask 6: Check if 3 exists in [1,2,3,4]"
  print (elementExists 3 [1,2,3,4])
  print (elementExists 5 [1,2,3,4])

  -- Task 7
  putStrLn "\nTask 7: Length of [10,20,30]"
  print (listLength [10,20,30])

  -- Task 8
  putStrLn "\nTask 8: Even numbers in [1..10]"
  print (filterEvens [1..10])

  -- Task 9
  putStrLn "\nTask 9: myMap (*2) [1..5]"
  print (myMap (*2) [1..5])

  -- Task 10
  putStrLn "\nTask 10: Digits of 12345"
  print (digits 12345)
