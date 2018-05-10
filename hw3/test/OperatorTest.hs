{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OperatorTest where

import Control.Monad.State
import qualified Data.Map.Strict as Map
import Test.Hspec

import Arithmetic
import Error
import Operators

operatorSpec :: Spec
operatorSpec = do
    it "mut x = 10" $
        runStateT (interpret [Init "x" (Lit 10)]) Map.empty `shouldBe` Right ((), Map.fromList [("x", 10)])
    it "x = 10" $
        runStateT (interpret [Set "x" (Lit 10)]) Map.empty `shouldBe` Left (InterpretError (NotInitialized "x") 1)
    it "mut x = 10 / 0" $
        runStateT (interpret [Init "x" (Lit 10 `Div` Lit 0)]) Map.empty `shouldBe` Left (InterpretError DivisionByZero 1)
    it "empty" $
        runStateT (interpret []) Map.empty `shouldBe` Right ((), Map.empty)
    it "Some steps" $
        runStateT (interpret [ Init "x" (Lit 10 `Add` Lit 11)
                             , Init "y" (Lit (-1) `Mul` Lit 3)
                             , Set "x" (Var "x" `Add` Var "y")
                             ]) Map.empty `shouldBe` Right ((), Map.fromList [ ("x", 18)
                                                                             , ("y", -3)
                                                                             ])
    it "Some steps with error" $
        runStateT (interpret [ Init "x" (Lit 10 `Add` Lit 11)
                             , Init "y" (Lit 3 `Sub` Lit 3)
                             , Set "x" (Var "x" `Div` Var "y")
                             , Init "z" (Var "x" `Mul` Var "y")
                             ]) Map.empty `shouldBe` Left (InterpretError DivisionByZero 3)

    it "Some steps with error II" $
        runStateT (interpret [ Init "x" (Lit 10 `Add` Lit 11)
                             , Init "y" (Lit 3 `Sub` Lit 3)
                             , Set "x" (Var "z" `Div` Var "y")
                             ]) Map.empty `shouldBe` Left (InterpretError (UninitializedVariable "z") 3)

    it "Some steps II" $
        runStateT (interpret [ Init "x" (Lit 10 `Add` Lit 11)   -- x = 21
                             , Init "y" (Lit 3 `Add` Var "x")   -- y = 24
                             , Set "x" (Var "x" `Add` Var "y")  -- x = 45
                             , Init "z" (Var "x" `Mul` Var "y") -- z = 1080
                             ]) Map.empty `shouldBe` Right ((), Map.fromList [ ("x", 45)
                                                                             , ("y", 24)
                                                                             , ("z", 1080)
                                                                             ])
