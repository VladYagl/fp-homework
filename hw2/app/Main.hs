module Main where

import ArithTest
import ParserTest
import Test.Tasty
import TypesTest

main :: IO ()
main = defaultMain (testGroup "All" [arithTestTree, parserTestTree, typesTestTree])
