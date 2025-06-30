-- HC2 - Haskell Chapter 2 Practical Tasks

-- Task 2: Function Type Signatures and Implementations
add :: Int -> Int -> Int
add x y = x + y

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

concatStrings :: String -> String -> String
concatStrings s1 s2 = s1 ++ s2

-- Task 3: Immutable Variables
myAge :: Int
myAge = 25

piValue :: Double
piValue = 3.14159

greeting :: String
greeting = "Hello, Haskell!"

isHaskellFun :: Bool
isHaskellFun = True

-- Task 4: Infix and Prefix
infixToPrefix1 = (+) 5 3
infixToPrefix2 = (*) 10 4
infixToPrefix3 = (&&) True False

prefixToInfix1 = 7 + 2
prefixToInfix2 = 6 * 5
prefixToInfix3 = True && False

-- Task 5: Defining and Using Functions
circleArea :: Float -> Float
circleArea r = pi * r * r

maxOfThree :: Int -> Int -> Int -> Int
maxOfThree x y z = max x (max y z)

-- Task 6: Int vs Integer
smallNumber :: Int
smallNumber = 2^62

bigNumber :: Integer
bigNumber = 2^127

-- Task 7: Boolean Expressions
boolExpr1 = True && True         -- Should be True
boolExpr2 = False || False       -- Should be False
boolExpr3 = not False            -- Should be True
boolExpr4 = 10 > 20              -- Should be False

-- Main
main :: IO ()
main = do
    putStrLn "=== HC2 Haskell Chapter 2 Practical Tasks ==="

    -- Task 1: Expected Types
    putStrLn "\nTask 1: Expected Types"
    putStrLn "42              :: Int"
    putStrLn "3.14            :: Fractional => Double or Float"
    putStrLn "\"Haskell\"       :: String"
    putStrLn "'Z'            :: Char"
    putStrLn "True && False  :: Bool"

    -- Task 2: Function Demos
    putStrLn "\nTask 2: Function Demos"
    print (add 4 5)
    print (isEven 6)
    print (concatStrings "Functional " "Programming")

    -- Task 3: Immutable Variables
    putStrLn "\nTask 3: Immutable Variables"
    print myAge
    print piValue
    print greeting
    print isHaskellFun
    -- Uncommenting the next line would cause an error because Haskell variables are immutable
    -- myAge = 30

    -- Task 4: Infix/Prefix
    putStrLn "\nTask 4: Infix to Prefix"
    print infixToPrefix1
    print infixToPrefix2
    print infixToPrefix3

    putStrLn "Task 4: Prefix to Infix"
    print prefixToInfix1
    print prefixToInfix2
    print prefixToInfix3

    -- Task 5: Functions
    putStrLn "\nTask 5: Circle Area and Max"
    print (circleArea 3.0)
    print (maxOfThree 10 99 50)

    -- Task 6: Int vs Integer
    putStrLn "\nTask 6: Int vs Integer"
    print smallNumber
    print bigNumber
    putStrLn "Evaluating 2^64 :: Int in GHCi may cause overflow on some systems."

    -- Task 7: Boolean Expressions
    putStrLn "\nTask 7: Boolean Expressions"
    print boolExpr1
    print boolExpr2
    print boolExpr3
    print boolExpr4
