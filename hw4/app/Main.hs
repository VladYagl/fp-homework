module Main where

import Control.Exception
import Control.Monad.Except
import Control.Monad.State
import Data.List.NonEmpty
import System.Environment

import FileSystem
import Walk

main :: IO ()
main = do
    path : _ <- getArgs
    fs <- scanFS path
    Left e <- runExceptT (runStateT (repl) (fs :| [], fileCount fs, dirCount fs))
    _ <- throwIO e
    return ()
