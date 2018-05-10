module VarTest where

import Control.Monad.State
import qualified Data.Map.Strict as Map
import Test.Hspec

import Error
import Variables

varSpec :: Spec
varSpec = do
    it "empty -> x = 10" $
        runStateT (initialize "x" 10) Map.empty `shouldBe` Right ((), Map.fromList [("x", 10)])
    it "AlreadyInitialized" $
        runStateT (initialize "x" 10) (Map.fromList [("x", 11)]) `shouldBe` Left (AlreadyInitialized "x")
    it "empty -> x = 10" $
        runStateT (set "x" 10) Map.empty `shouldBe` Left (NotInitialized "x")
    it "AlreadyInitialized" $
        runStateT (set "x" 10) (Map.fromList [("x", 11)]) `shouldBe` Right ((), Map.fromList [("x", 10)])
