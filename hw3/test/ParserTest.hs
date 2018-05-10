module ParserTest where

import Test.Hspec
import Text.Megaparsec

import Arithmetic
import Operators
import Parser

parserSpec :: Spec
parserSpec = do
    describe "Expression" $ do
        it "-10" $
            parse expression "" "-10" `shouldBe` Right (Lit (-10))
        it "Space" $
            parse expression "" "10\t+\n\n10" `shouldBe` Right (Lit 10 `Add` Lit 10)
        it "Sum" $
            parse expression "" "10+10+10" `shouldBe` Right (Lit 10 `Add` Lit 10 `Add` Lit 10)
        it "Let" $
            parse expression "" "(let x = 5 / 3 in x * x + 1)" `shouldBe`
                Right ("x" `Let` (Lit 5 `Div` Lit 3) $ Var "x" `Mul` Var "x" `Add` Lit 1)
        it "Brackets" $
            parse expression "" "123 * (-231 + 0) / 11" `shouldBe`
                Right (Lit 123 `Mul` (Lit (-231) `Add` Lit 0) `Div` Lit 11)
        it "Big One" $
            parse expression "" "10 + 10 * (let x = 5 / 3 in x * x + 1) - 10 * (2 + 3)" `shouldBe`
                Right (Lit 10 `Add` (Lit 10 `Mul` ("x" `Let` (Lit 5 `Div` Lit 3) $ Var "x" `Mul` Var "x" `Add` Lit 1))
                    `Sub` (Lit 10 `Mul` (Lit 2 `Add` Lit 3)))
    describe "Operators" $ do
        it "mut x = 10" $
            parse operator "" "mut x = 10" `shouldBe` Right (Init "x" (Lit 10))
        it "x = 10" $
            parse operator "" "x = 10" `shouldBe` Right (Set "x" (Lit 10))
