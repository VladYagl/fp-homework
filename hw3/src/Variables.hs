{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Variables
       ( initialize
       , set
       ) where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Strict as Map

import Arithmetic
import Error

initialize :: (MonadError LangError m, MonadState VarEnv m) => VarName -> Int -> m ()
initialize name value = do
    vars <- get
    if Map.member name vars
       then throwError $ AlreadyInitialized name
       else put (Map.insert name value vars)

set :: (MonadError LangError m, MonadState VarEnv m) => VarName -> Int -> m ()
set name value = do
    vars <- get
    if Map.member name vars
       then put (Map.insert name value vars)
       else throwError $ NotInitialized name
