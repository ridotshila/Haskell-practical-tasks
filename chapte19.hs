{-# LANGUAGE InstanceSigs #-}

module Main where
import Control.Applicative
import Control.Monad (forever, when, replicateM)
import Data.Foldable (sequenceA)

----------------------------------
-- HC19T1: Applicative Instance for Pair
----------------------------------
data Pair a = Pair a a deriving (Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

----------------------------------
-- HC19T2: addThreeApplicative
----------------------------------
addThreeApplicative :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
addThreeApplicative = liftA3 (\x y z -> x + y + z)

----------------------------------
-- HC19T3: safeProduct
----------------------------------
safeProduct :: [Maybe Int] -> Maybe Int
safeProduct = fmap product . sequenceA

----------------------------------
-- HC19T4: liftAndMultiply
----------------------------------
liftAndMultiply :: Maybe Int -> Maybe Int -> Maybe Int
liftAndMultiply = liftA2 (*)

----------------------------------
-- HC19T5: applyEffects with <*>
----------------------------------
applyEffects :: (IO Int, IO Int) -> IO Int
applyEffects (a, b) = (\x y -> x + y) <$> a <*> b

----------------------------------
-- HC19T6: repeatEffect
----------------------------------
repeatEffect :: IO () -> IO ()
repeatEffect = forever

----------------------------------
-- HC19T7: conditionalPrint
----------------------------------
conditionalPrint :: Bool -> String -> IO ()
conditionalPrint cond msg = when cond (putStrLn msg)

----------------------------------
-- HC19T8: discardSecond with <*
----------------------------------
discardSecond :: Applicative f => f a -> f b -> f a
discardSecond = (<*)

----------------------------------
-- HC19T9: pureAndApply
----------------------------------
pureAndApply :: IO ()
pureAndApply = do
  let f = pure (+3) <*> Just 7
  print f

----------------------------------
-- HC19T10: combineResults for Either
----------------------------------
combineResults :: Either String Int -> Either String Int -> Either String Int
combineResults = liftA2 (+)

----------------------------------
-- HC19T11: Applicative Instance for Wrapper
----------------------------------
newtype Wrapper a = Wrapper a deriving (Show)

instance Functor Wrapper where
  fmap f (Wrapper x) = Wrapper (f x)

instance Applicative Wrapper where
  pure = Wrapper
  (Wrapper f) <*> (Wrapper x) = Wrapper (f x)

----------------------------------
-- HC19T12: sumThreeApplicative
----------------------------------
sumThreeApplicative :: Either String Int -> Either String Int -> Either String Int -> Either String Int
sumThreeApplicative = liftA3 (\x y z -> x + y + z)

----------------------------------
-- HC19T13: whenApplicative
----------------------------------
whenApplicative :: Bool -> IO () -> IO ()
whenApplicative cond action = when cond action

----------------------------------
-- HC19T14: replicateEffect
----------------------------------
replicateEffect :: Int -> IO a -> IO [a]
replicateEffect = replicateM

----------------------------------
-- HC19T15: sequenceEffects
----------------------------------
sequenceEffects :: Applicative f => [f a] -> f [a]
sequenceEffects = sequenceA

----------------------------------
-- HC19T16: applyWithEffects
----------------------------------
applyWithEffects :: Applicative f => f (a -> b) -> f a -> f b
applyWithEffects = (<*>)

----------------------------------
-- HC19T17: simulateMaybeEffect
----------------------------------
simulateMaybeEffect :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
simulateMaybeEffect = liftA3 (\x y z -> x * y + z)

----------------------------------
-- HC19T18: combineEitherResults
----------------------------------
combineEitherResults :: Either String Int -> Either String Int -> Either String Int -> Either String Int
combineEitherResults = liftA3 (\x y z -> x + y + z)

----------------------------------
-- HC19T19: sequenceApplicative
----------------------------------
sequenceApplicative :: [Maybe a] -> Maybe [a]
sequenceApplicative = sequenceA

----------------------------------
-- HC19T20: replicateForever
----------------------------------
replicateForever :: IO () -> IO ()
replicateForever = forever

----------------------------------
-- Main for Demo
----------------------------------
main :: IO ()
main = do
  putStrLn "\n--- HC19T1: Pair Applicative ---"
  print $ (Pair (+1) (*2)) <*> (Pair 3 4)

  putStrLn "\n--- HC19T2: addThreeApplicative ---"
  print $ addThreeApplicative (Just 1) (Just 2) (Just 3)

  putStrLn "\n--- HC19T3: safeProduct ---"
  print $ safeProduct [Just 2, Just 3, Just 4]
  print $ safeProduct [Just 2, Nothing, Just 4]

  putStrLn "\n--- HC19T4: liftAndMultiply ---"
  print $ liftAndMultiply (Just 3) (Just 5)

  putStrLn "\n--- HC19T5: applyEffects ---"
  sumResult <- applyEffects (print 3 >> return 3, print 4 >> return 4)
  print sumResult

  putStrLn "\n--- HC19T7: conditionalPrint ---"
  conditionalPrint True "This is printed"
  conditionalPrint False "This is not printed"

  putStrLn "\n--- HC19T8: discardSecond ---"
  print $ discardSecond (Just "First") (Just "Second")

  putStrLn "\n--- HC19T9: pureAndApply ---"
  pureAndApply

  putStrLn "\n--- HC19T10: combineResults ---"
  print $ combineResults (Right 5) (Right 7)

  putStrLn "\n--- HC19T11: Wrapper Applicative ---"
  print $ (Wrapper (*2)) <*> (Wrapper 10)

  putStrLn "\n--- HC19T12: sumThreeApplicative ---"
  print $ sumThreeApplicative (Right 1) (Right 2) (Right 3)

  putStrLn "\n--- HC19T14: replicateEffect ---"
  results <- replicateEffect 3 (return "Hi")
  print results

  putStrLn "\n--- HC19T15: sequenceEffects ---"
  print $ sequenceEffects [Just 1, Just 2, Just 3]

  putStrLn "\n--- HC19T16: applyWithEffects ---"
  print $ applyWithEffects
