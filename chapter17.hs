{-# LANGUAGE DeriveGeneric #-}

module Main where
import Data.Semigroup
import Data.Monoid

----------------------------------
-- HC17T1: Severity Data Type
----------------------------------
data Severity = Low | Medium | High | Critical
  deriving (Show, Eq, Ord)

-- Semigroup: higher severity overrides lower
instance Semigroup Severity where
  (<>) = max

----------------------------------
-- HC17T2: Min and Max Newtypes
----------------------------------
newtype Min a = Min { getMin :: a } deriving (Show, Eq, Ord)
newtype Max a = Max { getMax :: a } deriving (Show, Eq, Ord)

instance (Ord a) => Semigroup (Min a) where
  (Min x) <> (Min y) = Min (min x y)

instance (Ord a) => Semigroup (Max a) where
  (Max x) <> (Max y) = Max (max x y)

----------------------------------
-- HC17T3: Monoid Instance for Severity
----------------------------------
instance Monoid Severity where
  mempty = Low
  mappend = (<>)

----------------------------------
-- HC17T4: Monoid Instance for Sum
----------------------------------
newtype Sum' = Sum' { getSum' :: Int } deriving (Show, Eq)

instance Semigroup Sum' where
  (Sum' x) <> (Sum' y) = Sum' (x + y)

instance Monoid Sum' where
  mempty = Sum' 0

----------------------------------
-- HC17T5: combineLists Function
----------------------------------
combineLists :: [Int] -> [Int] -> [Int]
combineLists = (<>)

----------------------------------
-- HC17T6: maxSeverity Function
----------------------------------
maxSeverity :: [Severity] -> Severity
maxSeverity = mconcat

----------------------------------
-- HC17T7: multiplyProducts Function
----------------------------------
newtype Product = Product { getProduct :: Int } deriving (Show, Eq)

instance Semigroup Product where
  (Product x) <> (Product y) = Product (x * y)

instance Monoid Product where
  mempty = Product 1

multiplyProducts :: [Product] -> Product
multiplyProducts = mconcat

----------------------------------
-- HC17T8: foldWithSemigroup Function
----------------------------------
foldWithSemigroup :: (Semigroup a) => [a] -> a
foldWithSemigroup = foldr1 (<>)

----------------------------------
-- HC17T9: Config Data Type + Semigroup
----------------------------------
data Config = Config
  { loggingLevel :: Int
  , timeout      :: Int
  , retries      :: Int
  } deriving (Show, Eq)

instance Semigroup Config where
  (Config l1 t1 r1) <> (Config l2 t2 r2) =
    Config (max l1 l2) (min t1 t2) (max r1 r2)

----------------------------------
-- HC17T10: Monoid Instance for Config
----------------------------------
instance Monoid Config where
  mempty = Config { loggingLevel = 0, timeout = maxBound, retries = 0 }

----------------------------------
-- Main for Demo
----------------------------------
main :: IO ()
main = do
  putStrLn "\n--- HC17T1–T3: Severity Semigroup & Monoid ---"
  print (Low <> Medium)      
  print (High <> Medium)     
  print (mconcat [Low, High, Critical, Medium])

  putStrLn "\n--- HC17T2: Min and Max ---"
  print (Min 5 <> Min 3)
  print (Max 5 <> Max 3)

  putStrLn "\n--- HC17T4: Sum' Monoid ---"
  print (Sum' 10 <> Sum' 15 <> mempty)

  putStrLn "\n--- HC17T5: combineLists ---"
  print (combineLists [1,2,3] [4,5])

  putStrLn "\n--- HC17T6: maxSeverity ---"
  print (maxSeverity [Low, Medium, Critical, High])

  putStrLn "\n--- HC17T7: multiplyProducts ---"
  print (multiplyProducts [Product 2, Product 3, Product 4])

  putStrLn "\n--- HC17T8: foldWithSemigroup ---"
  print (foldWithSemigroup [Product 2, Product 5, Product 10])

  putStrLn "\n--- HC17T9–T10: Config ---"
  let cfg1 = Config 2 100 3
  let cfg2 = Config 5 80 1
  print (cfg1 <> cfg2)
  print (mappend cfg1 mempty)
