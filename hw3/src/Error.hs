module Error
       ( LangError(..)
       , VarName
       ) where

import Control.Exception
import Data.Text
-- TODO: Effecient String
-- TODO: TESTS
type VarName = Text

data LangError = AlreadyInitialized VarName
               | NotInitialized VarName
               | UninitializedVariable VarName
               | DivisionByZero
               | InterpretError LangError Int
               deriving (Eq)

instance Show LangError where
    show (AlreadyInitialized name)    = "Already initialized: " ++ show name
    show (NotInitialized name)        = "Not initialized: " ++ show name
    show DivisionByZero               = "Division by zero"
    show (UninitializedVariable name) = "Uninitialized variable: " ++ show name
    show (InterpretError e pos)       = "Error '" ++ show e ++ "' at line " ++ show pos

instance Exception LangError

