import qualified Data.List as L
import qualified Data.Map as M
import System.IO

-- HC13T4 & HC13T5: Simulated Module Function (sumNonEmpty)
sumNonEmpty :: [Int] -> Int
sumNonEmpty [] = error "List is empty!"
sumNonEmpty xs = sum xs

main :: IO ()
main = do
  putStrLn "=== HC13T1: List Files (Simulated) ==="
  let files = ["main.hs", "data.csv", "log.txt", "summary.docx", "notes.txt"]
  mapM_ putStrLn files

  putStrLn "\n=== HC13T2: Filter Files by Substring ==="
  putStr "Enter keyword to filter files: "
  keyword <- getLine
  let filtered = filter (L.isInfixOf keyword) files
  putStrLn "Filtered files:"
  mapM_ putStrLn filtered

  putStrLn "\n=== HC13T3: Sort Filtered Files ==="
  let sortedFiltered = L.sort filtered
  putStrLn "Sorted filtered files:"
  mapM_ putStrLn sortedFiltered

  putStrLn "\n=== HC13T4 & HC13T5: Use sumNonEmpty ==="
  let nums = [10, 20, 30]
  putStrLn $ "Sum of " ++ show nums ++ ": " ++ show (sumNonEmpty nums)

  putStrLn "\n=== HC13T6: Convert File List to Map ==="
  let fileMap = M.fromList $ zip [1..] sortedFiltered
  print fileMap

  putStrLn "\n=== HC13T7: Using sumNonEmpty in Main ==="
  putStrLn $ "sumNonEmpty [1,2,3,4] = " ++ show (sumNonEmpty [1,2,3,4])

  putStrLn "\n=== HC13T8: Qualified Imports Example ==="
  let nums2 = [4, 1, 9, 2]
  putStrLn $ "Original list: " ++ show nums2
  putStrLn $ "Sorted (Data.List): " ++ show (L.sort nums2)
  putStrLn $ "Length (Prelude): " ++ show (length nums2)

  putStrLn "\n=== HC13T9: Renaming Module Namespace (Data.Map as M) ==="
  let studentScores = M.fromList [("Rofha", 90), ("Rokhe", 85), ("Wama", 78)]
  putStrLn "Student Scores Map:"
  print studentScores
  putStrLn "Wama's score:"
  print (M.lookup "Wama" studentScores)

  putStrLn "\n=== HC13T10: Combine Directory + List Actions ==="
  putStr "Enter keyword to search file list: "
  keyword2 <- getLine
  let finalResult = L.sort $ filter (L.isInfixOf keyword2) files
  putStrLn "Matching sorted files:"
  mapM_ putStrLn finalResult
