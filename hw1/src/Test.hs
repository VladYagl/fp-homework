{-# LANGUAGE RecordWildCards #-}

import Nat
import Town
import Tree

printTree :: Show a => Tree a -> Int -> String
printTree Leaf level          = ""
printTree Node{..} level =
    replicate level '|' ++ "Node: " ++ (show info) ++ "\n" ++ printTree left (level + 1) ++ printTree right (level + 1)

instance (Show a) => Show (Tree a) where
    show tree = printTree tree 0

main :: IO ()
main = do
    putStrLn (show (emptyTown))
    putStrLn (show ((fromInteger 11 :: Nat) >= fromInteger 2 * (fromInteger 5 :: Nat)))

    putStrLn (show(foldr (+) 100 (3 `delete` (10 `insert` (fromList [1, 2, 3, 4, 5] :: Tree Int)))))

