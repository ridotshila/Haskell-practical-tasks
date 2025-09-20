-- Main.hs
import System.IO
import Control.Exception
import Text.Read (readMaybe)
import qualified Data.Either as E

-------------------------------------------------
-- HC15T1: Handle Exceptions for File Reading and Velocity Calculation
-------------------------------------------------
velocity :: Double -> Double -> Double
velocity d t = d / t

hc15t1 :: IO ()
hc15t1 = do
    putStrLn "== HC15T1: Velocity with Exception Handling =="
    putStrLn "Enter distance:"
    dStr <- getLine
    putStrLn "Enter time:"
    tStr <- getLine
    let d = read dStr :: Double
        t = read tStr :: Double
    result <- try (evaluate (velocity d t)) :: IO (Either SomeException Double)
    case result of
        Left ex -> putStrLn $ "Error: " ++ show ex
        Right v -> putStrLn $ "Velocity = " ++ show v

-------------------------------------------------
-- HC15T2: Self-Driving AI Car System
-------------------------------------------------
selfDrivingCar :: String -> String
selfDrivingCar "red"    = "STOP"
selfDrivingCar "yellow" = "SLOW DOWN"
selfDrivingCar "green"  = "GO"
selfDrivingCar _        = "UNKNOWN SIGNAL"

hc15t2 :: IO ()
hc15t2 = do
    putStrLn "\n== HC15T2: Self-Driving Car =="
    putStrLn "Enter traffic light color:"
    color <- getLine
    putStrLn ("Car action: " ++ selfDrivingCar color)

-------------------------------------------------
-- HC15T3 & HC15T4: Custom Exception for Traffic Light + Handler
-------------------------------------------------
data TrafficLightException = InvalidLight String
    deriving Show
instance Exception TrafficLightException

checkLight :: String -> IO ()
checkLight "red"    = putStrLn "STOP"
checkLight "yellow" = putStrLn "SLOW DOWN"
checkLight "green"  = putStrLn "GO"
checkLight other    = throwIO (InvalidLight other)

hc15t3and4 :: IO ()
hc15t3and4 = do
    putStrLn "\n== HC15T3 & HC15T4: Custom Traffic Light Exception =="
    putStrLn "Enter traffic light:"
    color <- getLine
    checkLight color `catch` handler
  where
    handler :: TrafficLightException -> IO ()
    handler (InvalidLight c) = putStrLn ("Invalid traffic light: " ++ c)

-------------------------------------------------
-- HC15T5: Safe Division Using Maybe
-------------------------------------------------
safeDivMaybe :: Double -> Double -> Maybe Double
safeDivMaybe _ 0 = Nothing
safeDivMaybe x y = Just (x / y)

hc15t5 :: IO ()
hc15t5 = do
    putStrLn "\n== HC15T5: Safe Division with Maybe =="
    print (safeDivMaybe 10 2)
    print (safeDivMaybe 5 0)

-------------------------------------------------
-- HC15T6: Safe Input Parsing with readMaybe
-------------------------------------------------
hc15t6 :: IO ()
hc15t6 = do
    putStrLn "\n== HC15T6: Safe Input Parsing with readMaybe =="
    putStrLn "Enter a number:"
    input <- getLine
    case readMaybe input :: Maybe Int of
        Nothing -> putStrLn "Invalid number!"
        Just n  -> putStrLn ("You entered: " ++ show n)

-------------------------------------------------
-- HC15T7: Velocity with Optionals and Parsing Handling
-------------------------------------------------
velocityMaybe :: Double -> Double -> Maybe Double
velocityMaybe _ 0 = Nothing
velocityMaybe d t = Just (d / t)

hc15t7 :: IO ()
hc15t7 = do
    putStrLn "\n== HC15T7: Velocity with Maybe and Parsing =="
    putStrLn "Enter distance:"
    dStr <- getLine
    putStrLn "Enter time:"
    tStr <- getLine
    case (readMaybe dStr, readMaybe tStr) of
        (Just d, Just t) -> case velocityMaybe d t of
                                Just v  -> putStrLn ("Velocity = " ++ show v)
                                Nothing -> putStrLn "Error: Divide by zero"
        _ -> putStrLn "Invalid input!"

-------------------------------------------------
-- HC15T8: Division with Either for Detailed Errors
-------------------------------------------------
safeDivEither :: Double -> Double -> Either String Double
safeDivEither _ 0 = Left "Error: Division by zero"
safeDivEither x y = Right (x / y)

hc15t8 :: IO ()
hc15t8 = do
    putStrLn "\n== HC15T8: Division with Either =="
    print (safeDivEither 10 2)
    print (safeDivEither 5 0)

-------------------------------------------------
-- HC15T9: Try Function for File IO Exceptions
-------------------------------------------------
hc15t9 :: IO ()
hc15t9 = do
    putStrLn "\n== HC15T9: File IO Exception Handling with try =="
    result <- try (readFile "data.txt") :: IO (Either IOException String)
    case result of
        Left ex      -> putStrLn $ "File error: " ++ show ex
        Right content -> putStrLn ("File content:\n" ++ content)

-------------------------------------------------
-- HC15T10: Hybrid Error Handling with Either and IO
-------------------------------------------------
hc15t10 :: IO ()
hc15t10 = do
    putStrLn "\n== HC15T10: Hybrid Either + IO Error Handling =="
    putStrLn "Enter distance:"
    dStr <- getLine
    putStrLn "Enter time:"
    tStr <- getLine
    case (readMaybe dStr, readMaybe tStr) of
        (Just d, Just t) -> do
            case safeDivEither d t of
                Left err -> putStrLn err
                Right v  -> putStrLn ("Velocity = " ++ show v)
        _ -> putStrLn "Invalid input!"

-------------------------------------------------
-- MAIN
-------------------------------------------------
main :: IO ()
main = do
    hc15t1
    hc15t2
    hc15t3and4
    hc15t5
    hc15t6
    hc15t7
    hc15t8
    hc15t9
    hc15t10
