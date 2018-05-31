{-# LANGUAGE TemplateHaskell #-}

module Template where

import Language.Haskell.TH
import TemplateHaskell

data T = A     | B
data D = X T T | Z D
newtype M = J D

genShow ''T
genShow ''D
genShow ''Bool
genShow ''M

test :: IO ()
test = do
    putStrLn $(stringE . pprint =<< (genShow ''T))
    putStrLn ""
    putStrLn $(stringE . pprint =<< (genShow ''D))
    putStrLn ""
    putStrLn $(stringE . pprint =<< (genShow ''Bool))
    putStrLn ""
    putStrLn $(stringE . pprint =<< (genShow ''M))
