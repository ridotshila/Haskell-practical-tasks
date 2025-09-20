-- Main.hs
import Data.Char (toUpper)
import qualified Data.Map as M

-------------------------------------------------
-- HC16T1: Reverse a String
-------------------------------------------------
reverseString :: String -> String
reverseString = reverse

-------------------------------------------------
-- HC16T2: Palindrome Checker
-------------------------------------------------
isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

-------------------------------------------------
-- HC16T3: Factorial
-------------------------------------------------
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

-------------------------------------------------
-- HC16T4: Filter Even Numbers
-------------------------------------------------
filterEvens :: [Int] -> [Int]
filterEvens = filter even

-------------------------------------------------
-- HC16T5: Uppercase String
-------------------------------------------------
toUppercase :: String -> String
toUppercase = map toUpper

-------------------------------------------------
-- HC16T6: nth Fibonacci Number
-------------------------------------------------
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-------------------------------------------------
-- HC16T7: Element Existence in List
-------------------------------------------------
elementExists :: Eq a => a -> [a] -> Bool
elementExists = elem

-------------------------------------------------
-- HC16T8: Insertion Sort
-------------------------------------------------
insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
  where
    insert y [] = [y]
    insert y (z:zs)
        | y <= z    = y:z:zs
        | otherwise = z : insert y zs

-------------------------------------------------
-- HC16T9: Remove Duplicates from List
-------------------------------------------------
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
    | x `elem` xs = removeDuplicates xs
    | otherwise   = x : removeDuplicates xs

-------------------------------------------------
-- HC16T10: Character Frequency in String
-------------------------------------------------
charFrequency :: String -> M.Map Char Int
charFrequency s = M.fromListWith (+) [(c,1) | c <- s]

-------------------------------------------------
-- MAIN
-------------------------------------------------
main :: IO ()
main = do
    putStrLn "== HC16T1: Reverse String =="
    print (reverseString "haskell")

    putStrLn "\n== HC16T2: Palindrome Checker =="
    print (isPalindrome "madam")
    print (isPalindrome "hello")

    putStrLn "\n== HC16T3: Factorial =="
    print (factorial 5)

    putStrLn "\n== HC16T4: Filter Evens =="
    print (filterEvens [1..10])

    putStrLn "\n== HC16T5: Uppercase String =="
    print (toUppercase "haskell rocks")

    putStrLn "\n== HC16T6: Fibonacci =="
    print (fibonacci 10)

    putStrLn "\n== HC16T7: Element Exists =="
    print (elementExists 3 [1,2,3,4,5])
    print (elementExists 9 [1,2,3,4,5])

    putStrLn "\n== HC16T8: Insertion Sort =="
    print (insertionSort [5,3,8,1,2])

    putStrLn "\n== HC16T9: Remove Duplicates =="
    print (removeDuplicates [1,2,2,3,4,4,5,1])

    putStrLn "\n== HC16T10: Character Frequency =="
    print (charFrequency "haskellhaskell")
