{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Arithmetic
       ( Expr(..)
       , VarName
       , VarEnv
       , eval
       ) where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map.Strict as Map
-- import Data.Text

import Error

type VarEnv = Map.Map VarName Int

data Expr = Var VarName
          | Lit Int
          | Let VarName Expr Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving (Show, Eq)

eval :: (MonadError LangError m, MonadReader VarEnv m) => Expr -> m Int
eval = \case
    (Lit value) -> return value
    (Let name value expr) -> do
        varValue <- eval value
        local (Map.insert name varValue) (eval expr)
    (Var name) -> do
        r <- asks (Map.!? name)
        case r of
            Nothing -> throwError $ UninitializedVariable name
            Just x  -> return x
    (Add left right) -> apply left right (\l r -> return (l + r))
    (Sub left right) -> apply left right (\l r -> return (l - r))
    (Mul left right) -> apply left right (\l r -> return (l * r))
    (Div left right) -> apply left right division
  where
    division _ 0 = throwError DivisionByZero
    division l r = return (l `div` r)
    apply left right f = do
        l <- eval left
        r <- eval right
        f l r
