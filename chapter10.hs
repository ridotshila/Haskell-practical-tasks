{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import Data.List (sort)

-- Common Types
data PaymentMethod = Cash | Card | Cryptocurrency deriving (Eq, Ord, Show)

-- HC10T1: ShowSimple
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
data Box a = Empty | Has a deriving Show

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

-- HC10T6: Mutual recursion for Eq
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

-- HC11T1: WeAccept for Box
class WeAccept a where
  accept :: a -> Bool

instance WeAccept (Box a) where
  accept (Has _) = True
  accept Empty   = False

-- HC11T2: WeAccept for other types
instance WeAccept PaymentMethod where
  accept Cash = True
  accept Card = True
  accept _    = False

instance WeAccept Blockchain where
  accept Cardano = True
  accept _       = False

fancyFunction :: WeAccept a => [a] -> [a]
fancyFunction = filter accept

-- HC11T3: Container type class
class Container c a where
  isEmpty  :: c a -> Bool
  contains :: Eq a => a -> c a -> Bool
  replace  :: a -> c a -> c a

instance Container Box a where
  isEmpty Empty = True
  isEmpty _     = False
  contains x (Has y) = x == y
  contains _ _       = False
  replace x _        = Has x

-- HC11T4: Present container
data Present a = Wrap a | Unwrap deriving Show

instance Container Present a where
  isEmpty Unwrap = True
  isEmpty _      = False
  contains x (Wrap y) = x == y
  contains _ _        = False
  replace x _         = Wrap x

-- HC11T5: guessWhat'sInside
guessWhat'sInside :: (Container c a, Eq a) => a -> c a -> String
guessWhat'sInside x c = if contains x c then "Found it!" else "Not inside"

-- HC11T7: Ord for Box
instance Ord a => Ord (Box a) where
  compare Empty Empty = EQ
  compare Empty _     = LT
  compare _ Empty     = GT
  compare (Has x) (Has y) = compare x y

-- HC11T9: Length with Ord
data Length = M Float | Km Float deriving (Eq, Show)

instance Ord Length where
  compare (M x) (M y)   = compare x y
  compare (Km x) (Km y) = compare x y
  compare (M x) (Km y)  = compare x (y * 1000)
  compare (Km x) (M y)  = compare (x * 1000) y

-- HC11T10: sortContainers
sortContainers :: Ord a => [a] -> [a]
sortContainers = sort

main :: IO ()
main = do
  putStrLn "=== HC10 and HC11 ==="

  -- HC10T1: ShowSimple
  print $ showSimple Card

  -- HC10T2: Summable (explicit type annotation)
  print (sumUp [1,2,3] :: Int)

  -- HC10T3: Comparable
  print $ compareWith Ethereum Cardano

  -- HC10T4: Box Eq
  print $ Has 5 == Has 5

  -- HC10T5: ShowDetailed
  print $ showDetailed (User "Jane" "jane@demo.com")

  -- HC10T7: Convertible (explicit type annotation)
  print (convert Cash :: String)

  -- HC10T8: AdvancedEq
  print $ compareEquality Ethereum Ethereum

  -- HC10T9: MinMax
  print (minValue :: Int, maxValue :: Int)

  -- HC10T10: Concatenatable
  print $ concatWith "Hello, " "World!"

  -- HC11T2: fancyFunction for PaymentMethod
  print $ fancyFunction [Cash, Cryptocurrency]

  -- HC11T5: guessWhat'sInside
  print $ guessWhat'sInside 5 (Has 5 :: Box Int)

  -- HC11T10: sortContainers for Box
  print $ sortContainers [Has 3, Empty, Has 1]

  -- HC11T9: Length type comparison
  print $ M 1000 == Km 1
