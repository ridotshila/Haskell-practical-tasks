-- HC1 - Haskell Chapter 1 Practical Tasks

import Data.List (sortBy)
import Data.Ord (comparing, Down(..))

-- Task 1: Function Composition
double :: Int -> Int
double x = x * 2

increment :: Int -> Int
increment x = x + 1

doubleThenIncrement :: Int -> Int
doubleThenIncrement = increment . double

-- Task 2: Pure Function - Circle Area
circleArea :: Floating a => a -> a
circleArea r = pi * r * r

-- Task 3: Check if greater than 18
greaterThan18 :: Int -> Bool
greaterThan18 x = x > 18

-- Task 4: Compose player data functions
type Player = (String, Int)

extractPlayers :: [Player] -> [String]
extractPlayers players = map fst players

sortByScore :: [Player] -> [Player]
sortByScore = sortBy (comparing (Down . snd))

topThree :: [Player] -> [Player]
topThree = take 3

getTopThreePlayers :: [Player] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore

-- Task 5: Laziness - Infinite numbers
infiniteNumbers :: [Int]
infiniteNumbers = [1..]

firstN :: Int -> [Int]
firstN n = take n infiniteNumbers

-- Task 6: Type signature for sum
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

-- Task 7: Fahrenheit to Celsius
fToC :: Fractional a => a -> a
fToC f = (f - 32) * 5 / 9

-- Task 8: Higher-order function
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Main function to demo each task
main :: IO ()
main = do
    putStrLn "=== HC1 Haskell Tasks Demo ==="

    -- Task 1
    putStrLn "\nTask 1: doubleThenIncrement 3"
    print (doubleThenIncrement 3) -- 7

    -- Task 2
    putStrLn "\nTask 2: circleArea with radius 5"
    print (circleArea 5) -- 78.5398...

    -- Task 3
    putStrLn "\nTask 3: greaterThan18 20"
    print (greaterThan18 20) -- True

    -- Task 4
    let players = [("John", 10), ("Alice", 25), ("Bob", 15), ("Eve", 30)]
    putStrLn "\nTask 4: getTopThreePlayers"
    print (getTopThreePlayers players)

    -- Task 5
    putStrLn "\nTask 5: first 5 infinite numbers"
    print (firstN 5) -- [1,2,3,4,5]

    -- Task 6
    putStrLn "\nTask 6: addNumbers 3 and 4"
    print (addNumbers 3 4) -- 7

    -- Task 7
    putStrLn "\nTask 7: fToC 98.6"
    print (fToC 98.6) -- 37.0

    -- Task 8
    putStrLn "\nTask 8: applyTwice (+1) 3"
    print (applyTwice (+1) 3) -- 5
