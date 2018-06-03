{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE RecordWildCards #-}

module FileSystem
       ( FS(..)
       , scanFS
       , name
       , _dir
       , _file
       , contents
       , cd
       , ls
       , file
       , changeExtension
       , allNames
       ) where

-- import Control.Lens
-- import Control.Applicative
import Data.Monoid
import Lens.Micro.Platform
import System.Directory
import System.FilePath

data FS
    = Dir
          { _name     :: FilePath  -- название папки, не полный путь
          , _contents :: [FS]
          }
    | File
          { _name     :: FilePath  -- название файла, не полный путь
          }
    deriving (Show)

scanFS :: FilePath -> IO FS
scanFS root = do
    let (path, _file) = splitFileName root
    scanDir path _file

  where
    scanDir path _file = do
        let full = path ++ "/" ++ _file
        isDir <- doesDirectoryExist full
        if isDir
        then do
            list <- listDirectory full
            content <- traverse (scanDir full) list
            return Dir{ _name = _file
                      , _contents = content
                      }
        else
            return $ File _file

name :: Lens' FS FilePath
name = lens _name (\fs path -> fs { _name = path })

_file :: Traversal' FS FS
_file f = \case
    fs@File{..} -> id <$> f fs
    other -> pure other

_dir :: Traversal' FS FS
_dir f = \case
    fs@Dir{..} -> id <$> f fs
    other -> pure other

contents :: Traversal' FS [FS]
contents = _dir.content
  where
    content = lens _contents (\fs path -> fs { _contents = path })

cd :: FilePath -> Traversal' FS FS
cd dirName = contents.traversed.(filtered ((==) dirName . _name))

ls :: Traversal' FS FilePath
ls = contents.traversed.name

file :: FilePath -> Traversal' FS FilePath
file fileName = contents.traversed.(filtered ((==) fileName . _name)).name

changeExtension :: FS -> String -> FS
changeExtension fs extension = fs & contents.traversed._file.name %~ (flip replaceExtension extension)

allNames :: FS -> [FilePath]
allNames fs = fs ^.. allFiles
  where
    allFiles = contents.traversed.name <> contents.traversed.allFiles
