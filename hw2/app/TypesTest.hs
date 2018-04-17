module TypesTest where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.IO.Unsafe
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.Hspec

import Types

typesTestTree :: TestTree
typesTestTree = testGroup "Types" [
          testGroup "StringSum" [
                testProperty "String sum prop" stringSumProp
              , testProperty "String sum prop -- Nothign" stringSumNothingProp
              , unsafePerformIO (testSpec "stringSum" stringSumSpec)
          ]
        , testGroup "NonEmpty" [
              testProperty "left indentity" leftIdentityProp
            , testProperty "right indentity" rightIdentityProp
            , testProperty "Associativity" associativityProp
            , testProperty "Functor id" idFunctorProp
            , testProperty "Functor compose" composeFunctorProp
            , testProperty "Applicative id" idApplicativeProp
            , testProperty "Traversable" traversableProp
        ]
    ]

genIntList :: Gen [Int]
genIntList =
  let listLength = Range.linear 0 666
    in  Gen.list listLength Gen.enumBounded

stringSumProp :: Property
stringSumProp = property $ do
    intList <- forAll genIntList
    stringSum (unwords (map show intList)) === Just (sum intList)

stringSumNothingProp :: Property
stringSumNothingProp = property $ do
    intList <- forAll genIntList
    let goodList = unwords (map show intList)
    n <- forAll $ Gen.int (Range.singleton (length goodList))
    let badList = (\(a, b) -> a ++ "%" ++ b) (splitAt n goodList)
    stringSum badList === Nothing

stringSumSpec :: Spec
stringSumSpec = do
        it "Nothing" $
            stringSum "10 10  0 -1 20 -10 1+120312 -3 123" `shouldBe` Nothing
        it "1 element" $
            stringSum "      10     \n " `shouldBe` Just 10
        it "5 elements" $
            stringSum "      10     \n -1 \t\n 11\n\n\n3\t4" `shouldBe` Just 27
        it "last" $
            stringSum "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20" `shouldBe` Just 210

-- NonEmpty

leftIdentityProp :: Property
leftIdentityProp = property $ do
    a <- forAll Gen.bool
    let f x = not x :| [x]
    (return a >>= f) === f a

rightIdentityProp :: Property
rightIdentityProp = property $ do
    a <- forAll (Gen.filter (not . null) genIntList)
    let nonList = head a :| tail a
    (nonList >>= return) === nonList

associativityProp :: Property
associativityProp = property $ do
    a <- forAll (Gen.filter (not . null) genIntList)
    let nonList = head a :| tail a
    ((nonList >>= f) >>= g) === (nonList >>= (\x -> f x >>= g))
      where
        f x = x :| [x + 1, x + 2, x + 3]
        g x = (x * x) :| [x * 3, x * 4, x * 5, x * 6]

-- TODO: Generate functions
-- TODO: Delete all what is next!!!


idFunctorProp :: Property
idFunctorProp = property $ do
    a <- forAll (Gen.filter (not . null) genIntList)
    let nonList = head a :| tail a
    fmap id nonList === nonList

composeFunctorProp :: Property
composeFunctorProp = property $ do
    a <- forAll (Gen.filter (not . null) genIntList)
    let nonList = head a :| tail a
    fmap (f . g) nonList === (fmap f . fmap g) nonList
      where
        f x = (x + 13) * 39
        g x = (x * 2) + 234

idApplicativeProp :: Property
idApplicativeProp = property $ do
    a <- forAll (Gen.filter (not . null) genIntList)
    let nonList = head a :| tail a
    (pure id <*> nonList) === nonList

newtype Compose f g a = Compose { getCompose :: f (g a)  } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose x) = Compose (fmap (fmap f) x)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure x = Compose (pure (pure x))
    Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)

traversableProp :: Property
traversableProp = property $ do
    a <- forAll (Gen.filter (not . null) genIntList)
    let nonList = head a :| tail a
    traverse (Compose . fmap g . f) nonList === (Compose . fmap (traverse g) . traverse f) nonList
      where
        f :: Int -> Maybe Int
        f x = if odd x then Just (x + 1) else Nothing
        g :: Int -> Maybe Int
        g x = if x `mod` 100 == 0 then Nothing else Just (x * 30 + 1)
