module SimpleLensTest where

import Lens

import Test.Hspec

lensSpec :: Spec
lensSpec = do
    it "_1 view" $
        view _1 ("kcuf", [1, 2]) `shouldBe` "kcuf"
    it "_1 set" $
        set _1 "fuck" ("kcuf", [1, 2]) `shouldBe` ("fuck", [1, 2])
    it "_1 over" $
        over _1 reverse ("kcuf", [1, 2]) `shouldBe` ("fuck", [1, 2])
    it "_2 view" $
        view _2 ("kcuf", [1, 2]) `shouldBe` [1, 2]
    it "_2 set" $
        set _2 [4, 3, 2, 1] ("kcuf", [1, 2]) `shouldBe` ("kcuf", [4, 3, 2, 1])
    it "_2 view" $
        over _2 reverse ("kcuf", [1, 2]) `shouldBe` ("kcuf", [2, 1])
