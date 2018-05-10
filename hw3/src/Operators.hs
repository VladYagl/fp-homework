{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Operators
       ( Oper(..)
       , interpret
       ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map.Strict as Map

import Arithmetic
import Error
import Variables

data Oper = Init VarName Expr
          | Set VarName Expr
          | Print Expr
          | Read VarName
          | For VarName Expr Expr [Oper]
          deriving (Eq, Show)

interpret :: (MonadError LangError m, MonadState VarEnv m, MonadIO m) => [Oper] -> m ()
interpret = doSteps 1
  where
    doSteps :: (MonadError LangError m, MonadState VarEnv m, MonadIO m) => Int -> [Oper] -> m ()
    doSteps pos (x:xs) = do
        step x `catchError` (\e -> throwError $ InterpretError e pos)
        doSteps (pos + 1) xs
    doSteps _ [] = return ()

    step :: (MonadError LangError m, MonadState VarEnv m, MonadIO m) => Oper -> m ()
    step (Init name expr) = do
        vars <- get
        value <- runReaderT (eval expr) vars
        initialize name value
    step (Set name expr) = do
        vars <- get
        value <- runReaderT (eval expr) vars
        set name value
    step (Read name) = do
        value <- liftIO readLn
        initialize name value
    step (Print expr) = do
        vars <- get
        value <- runReaderT (eval expr) vars
        liftIO $ print value
    step (For var fromExpr toExpr opers) = do
        vars <- get
        from <- runReaderT (eval fromExpr) vars
        to <- runReaderT (eval toExpr) vars
        for from to vars
      where
        for cur to vars
          | cur <= to = do
              result <- runStateT (interpret opers) (Map.insert var cur vars)
              for (cur + 1) to (snd result)
          | otherwise = put vars
