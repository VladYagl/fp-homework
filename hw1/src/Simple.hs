module Simple
       ( order3
       , smartReplicate
       , contains
       , stringSum
       ) where

import Data.List

order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (x, y, z) = let [f, g, h] = sort [x, y, z] in (f, g, h)

smartReplicate :: [Int] -> [Int]
smartReplicate []       = []
smartReplicate (x : xs) = (replicate x x) ++ (smartReplicate xs)

contains :: Eq a => a -> [[a]] -> [[a]]
contains = filter . elem

stringSum :: String -> Int
stringSum = sum . map read . words
