-- HC9 - Haskell Chapter 9: Parameterized and Recursive Types

-- Task 1: Parametric type synonym
type Entity a = (a, String)  -- e.g., (Int, "address"), (String, "location")

-- Task 2: Parametric data type Box
data Box a = Empty | Has a deriving Show

-- Task 3: Add a number to a Box (if it contains one)
addN :: Num a => a -> Box a -> Box a
addN n (Has x) = Has (n + x)
addN _ Empty   = Empty

-- Task 4: Extract value or return default
extract :: a -> Box a -> a
extract _ (Has x) = x
extract def Empty = def

-- Task 5: Parametric Shape with color
data Shape a = Circle a Float | Rectangle a Float Float deriving Show
-- Example: Circle "red" 10.0

-- Task 6: Recursive Tweet type
data Tweet = Tweet {
  content :: String,
  likes :: Int,
  comments :: [Tweet]
} deriving Show

-- Task 7: Engagement: likes + engagement of all comments
engagement :: Tweet -> Int
engagement (Tweet _ l cs) = l + sum (map engagement cs)

-- Task 8: Recursive sequence type
data Sequence a = End | Node a (Sequence a) deriving Show

-- Task 9: Check if element is in Sequence
elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ End = False
elemSeq x (Node y rest) = x == y || elemSeq x rest

-- Task 10: Binary Search Tree
data BST a = Nil | NodeBST a (BST a) (BST a) deriving Show

-- Sample insert for testing BST
insertBST :: Ord a => a -> BST a -> BST a
insertBST x Nil = NodeBST x Nil Nil
insertBST x (NodeBST y left right)
  | x < y     = NodeBST y (insertBST x left) right
  | x > y     = NodeBST y left (insertBST x right)
  | otherwise = NodeBST y left right

-- Main function
main :: IO ()
main = do
  putStrLn "=== HC9 Haskell Chapter 9 Tasks ==="

  -- Task 1
  putStrLn "\nTask 1: Entity"
  print (("Alice", "123 Lane") :: Entity String)
  print ((42, "Blockchain") :: Entity Int)

  -- Task 2 & 3 & 4
  putStrLn "\nTask 2-4: Box operations"
  let box1 = Has 10
  let box2 = addN 5 box1
  let box3 = addN 3 (Empty :: Box Int)
  print box1
  print box2
  print box3
  print (extract 0 box2)
  print (extract 999 box3)

  -- Task 5
  putStrLn "\nTask 5: Shape with color"
  print (Circle "Red" 5.0)
  print (Rectangle "Blue" 4.0 6.0)

  -- Task 6 & 7
  putStrLn "\nTask 6 & 7: Tweet Engagement"
  let reply1 = Tweet "Nice!" 3 []
  let reply2 = Tweet "Interesting." 2 []
  let tweet1 = Tweet "Check out my post!" 10 [reply1, reply2]
  print tweet1
  print ("Engagement: " ++ show (engagement tweet1))

  -- Task 8 & 9
  putStrLn "\nTask 8 & 9: Sequence"
  let seq1 = Node 1 (Node 2 (Node 3 End))
  print seq1
  print (elemSeq 2 seq1)
  print (elemSeq 5 seq1)

  -- Task 10
  putStrLn "\nTask 10: Binary Search Tree"
  let bst = foldr insertBST Nil [10, 5, 15, 3, 7, 12, 18]
  print bst
