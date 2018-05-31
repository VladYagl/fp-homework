{-# LANGUAGE OverloadedStrings #-}

module ArithTest where

import Control.Monad.Reader
import qualified Data.Map.Strict as Map
import Test.Hspec

import Arithmetic
import Error

evalSpec :: Spec
evalSpec = do
        it "Lit" $
            runReaderT (eval (Lit 10)) Map.empty `shouldBe` Right 10
        it "Add" $
            runReaderT (eval (Lit 10  `Add` Lit 11)) Map.empty  `shouldBe` Right 21
        it "Sub" $
            runReaderT (eval (Lit 10  `Sub` Lit 11)) Map.empty  `shouldBe` Right (-1)
        it "Mul" $
            runReaderT (eval (Lit 3   `Mul` Lit 123)) Map.empty `shouldBe` Right 369
        it "Div" $
            runReaderT (eval (Lit 370 `Div` Lit 3)) Map.empty   `shouldBe` Right 123
        it "Division by zero" $
            runReaderT (eval (Lit 23 `Div` (Lit 12 `Sub` (Lit 3 `Mul` Lit 4)))) Map.empty `shouldBe` Left DivisionByZero
        it "Uninitialized variable" $
            runReaderT (eval $ Var "x") Map.empty `shouldBe` Left (UninitializedVariable "x")
        it "Some big evaluation" $
            runReaderT (eval (Lit 1 `Add` Lit 34 `Mul` Lit (-1) `Div` Lit 2 `Sub` Lit 23 `Mul` Lit 3)) Map.empty
                `shouldBe` Right (-123)
        it "Another big one" $
            runReaderT (eval $ Lit 30 `Sub` ("x" `Let` (Lit 10 `Add` Lit 20) $ Var "x" `Mul` Var "x")) Map.empty
                `shouldBe` Right (-870)

