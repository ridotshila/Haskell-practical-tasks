{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- HC10T1: ShowSimple
data PaymentMethod = Cash | Card | Cryptocurrency deriving (Eq, Ord, Show)

class ShowSimple a where
  showSimple :: a -> String

instance ShowSimple PaymentMethod where
  showSimple Cash = "Cash"
  showSimple Card = "Card"
  showSimple Cryptocurrency = "Cryptocurrency"

-- HC10T2: Summable
class Summable a where
  sumUp :: [a] -> a

instance Summable Int where
  sumUp = sum

-- HC10T3: Comparable
data Blockchain = Cardano | Ethereum | Bitcoin deriving Show

class Comparable a where
  compareWith :: a -> a -> Ordering

instance Comparable Blockchain where
  compareWith Cardano Cardano   = EQ
  compareWith Cardano _         = LT
  compareWith Ethereum Cardano  = GT
  compareWith Ethereum Ethereum = EQ
  compareWith Ethereum Bitcoin  = LT
  compareWith Bitcoin Bitcoin   = EQ
  compareWith Bitcoin _         = GT

-- HC10T4: Box Eq instance
data Box a = Empty | Has a deriving (Show)

instance Eq a => Eq (Box a) where
  Empty == Empty = True
  Has x == Has y = x == y
  _ == _ = False

-- HC10T5: ShowDetailed
data User = User { username :: String, email :: String }

class ShowDetailed a where
  showDetailed :: a -> String

instance ShowDetailed User where
  showDetailed (User name mail) = "User " ++ name ++ " <" ++ mail ++ ">"

-- HC10T6: Eq Blockchain (mutual recursion)
instance Eq Blockchain where
  a == b = not (a /= b)
  a /= b = case (a, b) of
    (Cardano, Cardano)   -> False
    (Ethereum, Ethereum) -> False
    (Bitcoin, Bitcoin)   -> False
    _ -> True

-- HC10T7: Convertible
class Convertible a b where
  convert :: a -> b

instance Convertible PaymentMethod String where
  convert = showSimple

-- HC10T8: AdvancedEq
class Eq a => AdvancedEq a where
  compareEquality :: a -> a -> Bool
  compareEquality x y = x == y

instance AdvancedEq Blockchain

-- HC10T9: MinMax
class MinMax a where
  minValue :: a
  maxValue :: a

instance MinMax Int where
  minValue = minBound
  maxValue = maxBound

-- HC10T10: Concatenatable
class Concatenatable a where
  concatWith :: a -> a -> a

instance Concatenatable [Char] where
  concatWith = (++)

-- Main function to run all tests
main :: IO ()
main = do
  putStrLn "=== Chapter 10 Tasks ==="

  -- HC10T1: ShowSimple
  putStrLn "\nHC10T1: ShowSimple"
  print $ showSimple Card

  -- HC10T2: Summable
  putStrLn "\nHC10T2: Summable"
  print (sumUp [10, 20, 30] :: Int)  -- Rofha's total amount

  -- HC10T3: Comparable
  putStrLn "\nHC10T3: Comparable"
  print $ compareWith Ethereum Cardano    -- Rokhe compares preferences

  -- HC10T4: Box Eq
  putStrLn "\nHC10T4: Box Eq"
  print $ Has "Wama" == Has "Wama"

  -- HC10T5: ShowDetailed
  putStrLn "\nHC10T5: ShowDetailed"
  let userRofha = User "Rofha" "rofha@block.io"
  print $ showDetailed userRofha

  -- HC10T6: Eq Blockchain
  putStrLn "\nHC10T6: Eq Blockchain"
  print $ Ethereum == Ethereum
  print $ Bitcoin == Ethereum

  -- HC10T7: Convertible
  putStrLn "\nHC10T7: Convertible"
  print (convert Cash :: String)

  -- HC10T8: AdvancedEq
  putStrLn "\nHC10T8: AdvancedEq"
  print $ compareEquality Bitcoin Bitcoin

  -- HC10T9: MinMax
  putStrLn "\nHC10T9: MinMax"
  print (minValue :: Int, maxValue :: Int)

  -- HC10T10: Concatenatable
  putStrLn "\nHC10T10: Concatenatable"
  print $ concatWith "Wama & " "Rokhe"
