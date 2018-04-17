module ParserTest where

import System.IO.Unsafe
import Test.Tasty
import Test.Tasty.Hspec

import Parser

parserTestTree :: TestTree
parserTestTree = testGroup "Parser" [
           unsafePerformIO (testSpec "Parser unit tests" parserSpec)
    ]

parserSpec :: Spec
parserSpec = do
    describe "eof" $ do
        it "is eof" $
            runParser eof "" `shouldBe` Just ((), "")
        it "is not eof" $
            runParser eof "some String" `shouldBe` Nothing
    describe "satisfy" $ do
        it "is odd" $
            runParser (satisfy odd) [1 :: Int, 2, 3] `shouldBe` Just (1, [2, 3])
        it "is not even" $
            runParser (satisfy even) [1 :: Int, 2, 3] `shouldBe` Nothing
    describe "element" $ do
        it "is element" $
            runParser (element 'a') "abc" `shouldBe` Just ('a', "bc")
        it "is not element" $
            runParser (element 'b') "abc" `shouldBe` Nothing
    describe "stream" $ do
        it "is stream" $
            runParser (stream "hello") "hello, world!" `shouldBe` Just ("hello", ", world!")
        it "is not stream" $
            runParser (stream "hello") "hellO, world!" `shouldBe` Nothing
    describe "brackets" $ do
        it "()" $
            runParser bracketSeq "()" `shouldBe` Just ((), "")
        it "empty" $
            runParser bracketSeq "" `shouldBe` Just ((), "")
        it "big one!" $
            runParser bracketSeq "(()(()(()(()))(())()()(()))())(()())(())" `shouldBe` Just ((), "")
        it ")" $
            runParser bracketSeq ")" `shouldBe` Nothing
        it "(" $
            runParser bracketSeq "(" `shouldBe` Nothing
        it ")(" $
            runParser bracketSeq ")(" `shouldBe` Nothing
        it "(()" $
            runParser bracketSeq "(()" `shouldBe` Nothing
        it "())" $
            runParser bracketSeq "())" `shouldBe` Nothing
        it "big bad one!" $
            runParser bracketSeq "((()((((())(())())(())()()()()((())()))))" `shouldBe` Nothing
    describe "integer" $ do
        it "1" $
            runParser integer "1" `shouldBe` Just (1, "")
        it "69 and more" $
            runParser integer "69+93" `shouldBe` Just (69, "+93")
        it "+420" $
            runParser integer "+420" `shouldBe` Just (420, "")
        it "-1337" $
            runParser integer "-1337\n-12" `shouldBe` Just (-1337, "\n-12")
        it "empty" $
            runParser integer "" `shouldBe` Nothing
        it "bad" $
            runParser integer "-+12" `shouldBe` Nothing
        it "empty" $
            runParser integer "-0000" `shouldBe` Just (0, "")
    describe "int lists" $ do
        it "empty" $
            runParser intLists "" `shouldBe` Nothing
        it "easy" $
            runParser intLists "1, 0, 0, 4, 1, 2, 3, 4" `shouldBe` Just ([[0], [], [1, 2, 3, 4]], "")
        it "small" $
            runParser intLists "1,0" `shouldBe` Just ([[0]], "")
        it "example" $
            runParser intLists "2, 1,+10  , 3,5,-7, 2" `shouldBe` Just ([[1, 10], [5, -7, 2]], "")
        it "big one!" $
            runParser intLists "\n\t 4  ,    -1\t\t,\t+2,\n3,-5,2,100,200\t,\n0, hi!"
                `shouldBe` Just ([[-1, 2, 3, -5], [100, 200], []], ", hi!")
        it "empty lists" $
            runParser intLists "0, 0, 0" `shouldBe` Just ([[], [], []], "")
        it "not enough" $
            runParser intLists "1" `shouldBe` Nothing
        it "bad" $
            runParser intLists "1 0" `shouldBe` Nothing
        it "bad" $
            runParser intLists ",1 0" `shouldBe` Nothing
        it "bad" $
            runParser intLists "1,,0" `shouldBe` Nothing
