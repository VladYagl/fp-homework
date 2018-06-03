{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}

module Walk where

import Control.Exception
import Control.Monad.Except
import Control.Monad.State
import Data.List (intercalate)
import Data.List.NonEmpty hiding (length)
import Lens.Micro.Platform
import Prelude hiding (head, last, map, reverse)

import FileSystem

type Stack = (NonEmpty FS, Int, Int)

data FileError = NoSuchPath FilePath | UpRoot deriving (Show)

instance Exception FileError

fileCount :: FS -> Int
fileCount fs = length $ fs ^.. contents.traversed._file

dirCount :: FS -> Int
dirCount fs = length $ fs ^.. contents.traversed._dir

changeDir :: (MonadState Stack m, MonadError FileError m) => FilePath -> m ()
changeDir path = do
    (stack, files, dirs) <- get
    case head stack ^? cd path of
        Just new -> put (new <| stack, files + fileCount new, dirs + dirCount new)
        Nothing  -> throwError $ NoSuchPath path

upDir :: (MonadState Stack m, MonadError FileError m) => m ()
upDir = do
    (old :| stack, files, dirs) <- get
    case nonEmpty stack of
        Just newStack -> put (newStack, files - fileCount old, dirs - dirCount old)
        Nothing       -> throwError UpRoot

listDir :: (MonadState Stack m, MonadError FileError m) => m [FilePath]
listDir = do
    (root :| _, _, _) <- get
    return $ root ^.. ls

repl :: (MonadState Stack m, MonadError FileError m, MonadIO m) => m ()
repl = do
    (stack, files, dirs) <- get
    liftIO . putStrLn . (++ "\"") . ("You in \"" ++) . (intercalate "/") . toList . reverse $ map (^. name) stack
    let root = (last stack) ^. name
    liftIO . putStrLn $ "Files from root \"" ++ root ++ "\": " ++ (show files)
    liftIO . putStrLn $ "Directories from root \"" ++ root ++ "\": " ++ (show dirs)
    liftIO $ putStr "> "
    command <- liftIO getLine
    case words command of
        ["cd", path] -> changeDir path `catchError` (liftIO . print)
        ["up"]       -> upDir `catchError` (liftIO . print)
        ["ls"]       -> do
                            list <- listDir
                            liftIO $ mapM_ (putStrLn . ("\t" ++)) list
        _            -> liftIO $ putStrLn "Pls don't do it!"
    repl
