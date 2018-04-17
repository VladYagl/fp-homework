module ArithTest where

import Hedgehog hiding (eval)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.IO.Unsafe
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.Hspec

import Arithmetic

arithTestTree :: TestTree
arithTestTree = testGroup "Arithmetic" [
          unsafePerformIO (testSpec "eval" evalSpec)
        , testProperty "bin" binProp
        , testProperty "len" lenProp
    ]

evalSpec :: Spec
evalSpec = do
        it "Const" $
            eval (Const 10) `shouldBe` Right 10
        it "Add" $
            eval (Const 10  `Add` Const 11)  `shouldBe` Right 21
        it "Sub" $
            eval (Const 10  `Sub` Const 11)  `shouldBe` Right (-1)
        it "Mul" $
            eval (Const 3   `Mul` Const 123) `shouldBe` Right 369
        it "Div" $
            eval (Const 370 `Div` Const 3)   `shouldBe` Right 123
        it "Pow" $
            eval (Const 2   `Pow` Const 10)  `shouldBe` Right 1024
        it "Division by zero" $
            eval (Const 23 `Div` (Const 12 `Sub` (Const 3 `Mul` Const 4))) `shouldBe` Left DivisionByZero
        it "Pow to negative" $
            eval (Const 10 `Add` (Const 12 `Pow` (Const 31 `Sub` (Const 2 `Pow` Const 5)))) `shouldBe` Left NegativePow
        it "Some big evaluation" $
            eval (Const 1 `Add` Const 34 `Mul` Const (-1) `Div` Const 2 `Sub` Const 23 `Pow` Const 3) `shouldBe` Right (-68921)

genSeq :: Int -> Gen [Int]
genSeq len = Gen.list (Range.singleton len) (Gen.element [0, 1])

binProp :: Property
binProp = property $ do
    n <- forAll (Gen.int (Range.singleton 10))
    binseq <- forAll (genSeq n)
    (binseq `elem` bin n) === True

lenProp :: Property
lenProp = property $ do
    n <- forAll (Gen.int (Range.singleton 12))
    length (bin n) === 2 ^ n
