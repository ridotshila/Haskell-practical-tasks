-- HC9T1: Parametric Type Synonym
type Address = String
type Entity a = (a, Address)

-- HC9T2: Parametric Box Data Type
data Box a = Empty | Has a
    deriving (Show)

-- HC9T3: Add value to Box
addN :: Int -> Box Int -> Box Int
addN n (Has x) = Has (x + n)
addN _ Empty   = Empty

-- HC9T4: Extract value from Box
extract :: a -> Box a -> a
extract def (Has x) = x
extract def Empty   = def

-- HC9T5: Shape with record syntax
data Shape a = Circle { color :: a }
             | Rectangle { color :: a }
    deriving (Show)

-- HC9T6: Recursive Tweet Type
data Tweet = Tweet
    { content  :: String
    , likes    :: Int
    , comments :: [Tweet]
    } deriving (Show)

-- HC9T7: Engagement Calculator
engagement :: Tweet -> Int
engagement (Tweet _ l cs) = l + sum (map engagement cs)

-- HC9T8: Recursive Sequence Type
data Sequence a = Nil | Node a (Sequence a)
    deriving (Show)

-- HC9T9: Check for element in Sequence
elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ Nil = False
elemSeq x (Node y ys)
    | x == y    = True
    | otherwise = elemSeq x ys

-- HC9T10: Binary Search Tree
data BST a = EmptyTree
           | TreeNode a (BST a) (BST a)
    deriving (Show)

-- ✅ Unified main function to test all
main :: IO ()
main = do
    putStrLn "--- HC9T1: Entity ---"
    let rofhaEntity :: Entity String
        rofhaEntity = ("Rofha", "123 Tech Lane")
    print rofhaEntity

    putStrLn "\n--- HC9T2, HC9T3, HC9T4: Box ---"
    let boxRokhe = Has 10
    let boxWama = Empty :: Box Int
    print (addN 5 boxRokhe)         -- Has 15
    print (addN 3 boxWama)          -- Empty
    print (extract 100 boxRokhe)    -- 10
    print (extract 200 boxWama)     -- 200

    putStrLn "\n--- HC9T5: Shapes ---"
    let shape1 = Circle "Red (Rofha)"
    let shape2 = Rectangle "Blue (Rokhe)"
    print shape1
    print shape2

    putStrLn "\n--- HC9T6, HC9T7: Tweet & Engagement ---"
    let comment1 = Tweet "Rofha liked it" 2 []
    let comment2 = Tweet "Rokhe retweeted" 3 []
    let mainTweet = Tweet "Wama posted!" 5 [comment1, comment2]
    print mainTweet
    putStrLn ("Engagement: " ++ show (engagement mainTweet))  -- 10

    putStrLn "\n--- HC9T8, HC9T9: Sequence ---"
    let seq1 = Node "Rofha" (Node "Rokhe" (Node "Wama" Nil))
    print seq1
    putStrLn $ "Contains 'Wama'? " ++ show (elemSeq "Wama" seq1) -- True
    putStrLn $ "Contains 'Zizi'? " ++ show (elemSeq "Zizi" seq1) -- False

    putStrLn "\n--- HC9T10: Binary Search Tree ---"
    let bst = TreeNode "Rokhe"
                (TreeNode "Rofha" EmptyTree EmptyTree)
                (TreeNode "Wama" EmptyTree EmptyTree)
    print bst
