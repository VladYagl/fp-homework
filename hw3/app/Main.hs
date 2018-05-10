module Main where

import Control.Exception
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Strict as Map
-- import Data.Text hiding (head)
import qualified Data.Text.IO as Text
import System.Environment
import Text.Megaparsec

import Operators
import Parser

-- TODO: This looks like shit

main :: IO ()
main = do
    args <- getArgs
    input <- Text.readFile (head args)
    let operators = parse file (head args) input
    case operators of
        Left e  -> throwIO e
        Right opers -> do
            result <- runExceptT $ runStateT (interpret opers) Map.empty
            case result of
                Left e  -> throwIO e
                Right _ -> return ()
