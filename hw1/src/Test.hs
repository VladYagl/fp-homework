{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test
      ( foldTest
      , natTest
      , townTest
      , treeTest
      , monoidTest
      , weekTest
      , listTest
      , simpleTest
      ) where

import Data.Monoid
import System.Random (newStdGen, randomRs)

import Fold
import Lists
import Monoids
import Nat
import Simple
import Town
import Tree
import Weekday

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen


printTree :: Show a => Tree a -> Int -> String
printTree Leaf _         = "Leaf\n"
printTree Node{..} level =
    replicate level '|' ++ "Node: " ++ show info ++ "\n" ++ printTree left (level + 1) ++ printTree right (level + 1)

instance (Show a) => Show (Tree a) where
    show t = printTree t 0

deriving instance Show a => Show (NonEmpty a)
deriving instance Show Name
deriving instance Show Builder

monoidTest :: IO()
monoidTest = do
    print $ maybeConcat [Just [1,2,3], Nothing, Just [4,5 :: Int]]
    print $ eitherConcat [Left (Sum 3), Right [1,2,3], Left (Sum 5 :: Sum Int), Right [4,5 :: Int]]
    print $ Name "root" <> Name "server"
    print (fromString "test String" :: Builder)
    print $ toString (fromString "test String" :: Builder)

foldTest :: IO()
foldTest = do
    print $ foldr (-) (1000 :: Int) (Pair 1 100)
    print $ foldr (-) (1000 :: Int) (1 :| [10, 100])
    print $ splitOn '/' "path/to/file"
    print $ splitOn '/' "/path//to/file/"
    print $ joinWith '/' ("path" :| ["to", "file"])
    print $ joinWith '|' . splitOn '/' $ "path/to/file"

weekTest :: IO()
weekTest = do
    print $ nextDay Monday
    print $ afterDays Friday 10
    print $ daysToParty Saturday
    print $ map isWeekend [Monday, Friday, Sunday, Saturday]
    print $ map daysToParty [Monday, Friday, Sunday, Saturday]
    print $ map nextDay [Monday, Friday, Sunday, Saturday]

unbox :: BuildResult a -> a
unbox (Success x)       = x
unbox (Failure message) = error ("Failed unbox: " ++ message)

townTest :: IO()
townTest = do
    print $ town Nothing Library Nothing Nothing [unbox $ house 3]
    print $ town Nothing Library (Just Lord) (Just Walls) [unbox $ house 3]
    print $ town Nothing Library Nothing Nothing [unbox $ house 3]
    putStrLn "\n\n"
    let t = emptyTown
    print t
    let t1 = unbox $ buildInTown t Library
    print t1
    let t3 = unbox $ buildHouse (unbox $ buildHouse (unbox $ buildHouse t1 4) 4) 4
    print t3
    print $ buildWalls t3 Walls
    print $ addLord t3 Lord
    let t2 = unbox $ buildCastle t3 Castle
    print t2
    let t5 = unbox $ addLord t2 Lord
    print t5
    let t6 = unbox $ buildWalls t5 Walls
    print t6

natTest :: IO()
natTest = do
    let i11 = 11 :: Nat
    let i2  = 2 :: Nat
    let i5 = 5 :: Nat
    let i3 = 3 :: Nat
    print (i11 >= i2 * i5)
    print (toInteger (i11 `quot` i3) == 11 `quot` 3)
    print (toInteger (i11 `mod` i3) == 11 `mod` 3)

treeTest :: IO()
treeTest = do
    let tree = 3 `delete` (10 `insert` (fromList [1, 2, 3, 4, 5] :: Tree Int))
    print (foldr (+) 100 tree)
    print (find 2 tree)
    putStrLn "\n"
    print $ 1 `delete` (fromList [1] :: Tree Int)

listTest :: IO()
listTest = do
    example <- randomIntList 5 (-10) 10
    print $ mergeSort example
    print $ deleteAt 2 [Monday, Friday, Sunday, Saturday]

simpleTest :: IO()
simpleTest = do
    print $ map stringSum ["100\n\t-3", "1", "1 2 3", " 1", "1 ", "\t1\t", "\t12345\t", "010 020 030"
                , " 123 456 789 ", "-1", "-1 -2 -3", "\t-12345\t", " -123 -456 -789 "
                            , "\n1\t\n3   555  -1\n\n\n-5", "123\t\n\t\n\t\n321 -4 -40"]

    print $ contains 3 [[1..5], [2,0], [3,4:: Int]]
    print $ smartReplicate [1,2,3]
    print $ order3 (5, 2, 10 :: Int)

