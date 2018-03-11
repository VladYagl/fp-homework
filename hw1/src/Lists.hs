module Lists
       ( randomIntList
       , deleteAt
       , mergeSort
       ) where

import System.Random (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

deleteAt :: Int -> [a] -> (a, [a])
deleteAt _ []         = error "No such element"
deleteAt 0 (x : xs)   = (x, xs)
deleteAt pos (x : xs) = let (y, result) = deleteAt (pos - 1) xs in (y, x : result)

mergeSort :: Ord a => [a] -> [a]
mergeSort = head . impl . map (\x -> [x])
  where
    impl :: (Ord a) => [[a]] -> [[a]]
    impl []       = []
    impl (x : []) = [x]
    impl l        = impl (mergeAll l)

    mergeAll :: (Ord a) => [[a]] -> [[a]]
    mergeAll []           = []
    mergeAll (x : [])     = [x]
    mergeAll (x : y : xs) = [merge x y] ++ mergeAll xs

    merge :: (Ord a) => [a] -> [a] -> [a]
    merge [] ys             = ys
    merge xs []             = xs
    merge (x : xs) (y : ys)
        | x < y     = x : merge xs (y : ys)
        | otherwise = y : merge (x : xs) ys
