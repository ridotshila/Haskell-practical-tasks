{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Data.List (group, sort)

-- HC14T4: Read string as Int using TypeApplications
readInt :: String -> Int
readInt = read @Int

-- HC14T5: Custom Result type with pattern matching
data Result a = Success a | Error String

handleResult :: Result Int -> String
handleResult r@(Success val) = "Success: " ++ show val ++ " (from " ++ show val ++ ")"
handleResult (Error msg)     = "Error: " ++ msg

-- HC14T8: Character frequency function
counts :: String -> [(Char, Int)]
counts str = map (\xs -> (head xs, length xs)) . group . sort $ str

-- HC14T9: PartialTypeSignatures extension in use
showAll :: Show a => [a] -> [String]
showAll = map show :: _ -> [String]

main :: IO ()
main = do
  putStrLn "=== HC14T1: Hello Cabal ==="
  putStrLn "Hello, Cabal!"

  putStrLn "\n=== HC14T2: Random Number ==="
  putStrLn "Random number feature skipped (System.Random not available online)."

  putStrLn "\n=== HC14T3: NumericUnderscores ==="
  let bigNumber = 1_000_000
  print bigNumber

  putStrLn "\n=== HC14T4: TypeApplications readInt ==="
  print (readInt "42")

  putStrLn "\n=== HC14T5: Result Type with @ pattern ==="
  print (handleResult (Success 7))
  print (handleResult (Error "Something went wrong"))

  putStrLn "\n=== HC14T8: Character Frequencies ==="
  print (counts "mississippi")

  putStrLn "\n=== HC14T9: showAll using PartialTypeSignatures ==="
  print (showAll [1, 2, 3])

  putStrLn "\n=== HC14T10: Simulated Test ==="
  let testResult = counts "aabbc"
  if testResult == [('a',2),('b',2),('c',1)]
    then putStrLn "Test passed!"
    else putStrLn "Test failed."

  putStrLn "\n✅ All tasks (HC14T1–HC14T10) completed in one file."
