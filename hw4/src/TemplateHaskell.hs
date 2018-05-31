{-# LANGUAGE TemplateHaskell #-}

module TemplateHaskell
       ( ShowText(..)
       , chooseByIndices
       , chooseByIndices2
       , genShow
       ) where

import Control.Monad
import qualified Data.Text as Text
import Language.Haskell.TH

chooseByIndices :: Int -> [Int] -> Q Exp
chooseByIndices n list = do
    names <- replicateM n (newName "x")
    lamE [tupP $ map varP names] (tupE $ map (varE . (names !!)) list)

-- usage :
-- $( chooseByIndices2 [3, 2, 1] [| (1, "hello", [1,2,3], Just 1) |] )
-- ¯\_(ツ)_/¯ can't do :
-- x = (1, 2)
-- $( chooseByIndices2 [0] [| x |] )
chooseByIndices2 :: [Int] -> Q Exp -> Q Exp
chooseByIndices2 list tuple = do
    tupleExp <- tuple
    case tupleExp of
        TupE values -> tupE $ map (pure . (values !!)) list
        _           -> undefined

class ShowText a where
    showText :: a -> Text.Text

genShow :: Name -> Q [Dec]
genShow typeName = do
    TyConI t <- reify typeName
    case t of
        DataD _ _ _ _ constructors _   -> impl constructors
        NewtypeD _ _ _ _ constructor _ -> impl [constructor]
        _                              -> undefined

  where
    impl constructors = do
        let fun = funD (mkName "showText") (map genClause constructors)
        sequence [instanceD (return []) [t|ShowText $(conT typeName)|] [fun]]

    genClause (NormalC name fields) = do
        fieldNames <- replicateM (length fields) (newName "x")

        let pat = conP name (map varP fieldNames)
            fieldShow = listE $ map (\var -> [|Text.cons ' ' $ showText $(varE var)|]) fieldNames
            body = normalB [|
                                Text.cons '('
                              $ flip Text.snoc ')'
                              $ Text.concat
                              $ Text.pack $(stringE $ nameBase name)
                              : $(fieldShow)
                           |]
        clause [pat] body []
    genClause _ = undefined
