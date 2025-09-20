{-# LANGUAGE DeriveFunctor #-}

module Main where
import Data.Char (toLower)

----------------------------------
-- HC18T1: mapToLower Function
----------------------------------
mapToLower :: String -> String
mapToLower = fmap toLower

----------------------------------
-- HC18T2: Functor Instance for Tree
----------------------------------
data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Show, Functor)

----------------------------------
-- HC18T3: incrementTreeValues
----------------------------------
incrementTreeValues :: Num a => Tree a -> Tree a
incrementTreeValues = fmap (+1)

----------------------------------
-- HC18T4: mapToBits
----------------------------------
mapToBits :: [Bool] -> [Char]
mapToBits = fmap (\b -> if b then '1' else '0')

----------------------------------
-- HC18T5: Functor Instance for Either
----------------------------------
-- Already in Prelude, but we re-implement:
instance Functor (Either e) where
  fmap _ (Left e)  = Left e
  fmap f (Right x) = Right (f x)

----------------------------------
-- HC18T6: applyToMaybe
----------------------------------
applyToMaybe :: (a -> b) -> Maybe a -> Maybe b
applyToMaybe = fmap

----------------------------------
-- HC18T7: fmapTuple
----------------------------------
fmapTuple :: (b -> c) -> (a, b) -> (a, c)
fmapTuple = fmap

----------------------------------
-- HC18T8: identityLawCheck
----------------------------------
identityLawCheck :: (Eq (f a), Functor f) => f a -> Bool
identityLawCheck x = fmap id x == x

----------------------------------
-- HC18T9: compositionLawCheck
----------------------------------
compositionLawCheck :: (Eq (f c), Functor f)
                    => (b -> c) -> (a -> b) -> f a -> Bool
compositionLawCheck f g x =
  fmap (f . g) x == (fmap f . fmap g) x

----------------------------------
-- HC18T10: nestedFmap
----------------------------------
nestedFmap :: (a -> b) -> [[Maybe a]] -> [[Maybe b]]
nestedFmap = fmap . fmap . fmap

----------------------------------
-- Main for Demo
----------------------------------
main :: IO ()
main = do
  putStrLn "\n--- HC18T1: mapToLower ---"
  print (mapToLower "HELLO FunCtOrS")

  putStrLn "\n--- HC18T2 & HC18T3: Tree Functor + Increment ---"
  let tree = Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty)
  print tree
  print (incrementTreeValues tree)

  putStrLn "\n--- HC18T4: mapToBits ---"
  print (mapToBits [True, False, True, True])

  putStrLn "\n--- HC18T5: Either Functor ---"
  print (fmap (+1) (Right 10 :: Either String Int))
  print (fmap (+1) (Left "error" :: Either String Int))

  putStrLn "\n--- HC18T6: applyToMaybe ---"
  print (applyToMaybe (*2) (Just 10))
  print (applyToMaybe (*2) Nothing)

  putStrLn "\n--- HC18T7: fmapTuple ---"
  print (fmapTuple length ("tag", "hello"))

  putStrLn "\n--- HC18T8: identityLawCheck ---"
  print (identityLawCheck (Just 5))
  print (identityLawCheck [1,2,3])

  putStrLn "\n--- HC18T9: compositionLawCheck ---"
  print (compositionLawCheck (+1) (*2) (Just 3))
  print (compositionLawCheck (++"!") reverse ["hi","yo"])

  putStrLn "\n--- HC18T10: nestedFmap ---"
  print (nestedFmap (+1) [[Just 1, Nothing], [Just 2, Just 3]])
