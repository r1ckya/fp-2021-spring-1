{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.List as L (foldl')

or' :: [Bool] -> Bool
or' = foldr (||) True

length' :: [a] -> Int
length' = foldr accum 0
  where
    accum :: a -> Int -> Int
    accum x resRight = resRight + 1

maximum' :: Ord a => [a] -> Maybe a
maximum' = foldr accum Nothing
  where
    accum :: Ord a => a -> Maybe a -> Maybe a
    accum x Nothing  = Just x
    accum x (Just y) | x > y     = Just x
                     | otherwise = Just y

filter' :: forall a. (a -> Bool) -> [a] -> [a]
filter' p = foldr accum []
  where
    accum :: a -> [a] -> [a]
    accum x l | p x       = x : l
              | otherwise = l

map' :: forall a b. (a -> b) -> [a] -> [b]
map' f = foldr accum []
  where
    accum :: a -> [b] -> [b]
    accum x l = f x : l

head' :: [a] -> Maybe a
head' = foldr accum Nothing
  where
    accum :: a -> Maybe a -> Maybe a
    accum x _ = Just x

last' :: [a] -> Maybe a
last' = foldr accum Nothing
  where
    accum :: a -> Maybe a -> Maybe a
    accum _ (Just x) = Just x
    accum x Nothing  = Just x

take' :: Int -> [a] -> [a]
take' n = snd . L.foldl' accum (0, [])
  where
    accum :: (Int, [a]) -> a -> (Int, [a])
    accum (len, l) x | len == n  = (len, l)
                     | otherwise = (len + 1, l <> [x])

take'' :: Int -> [a] -> [a]
take'' n l = (foldr accum ini l) n
  where
    accum :: a -> (Int -> [a]) -> (Int -> [a])
    accum x tailTake = newTake
      where
        newTake :: Int -> [a]
        newTake n | n <= 0    = []
                  | otherwise = x : tailTake (n - 1)

    ini :: Int -> [a]
    ini _ = []