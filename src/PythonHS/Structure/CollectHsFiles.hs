module PythonHS.Structure.CollectHsFiles (collectHsFiles) where

import Control.Monad (forM)
import PythonHS.Structure.IsHaskellFile (isHaskellFile)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

collectHsFiles :: FilePath -> IO [FilePath]
collectHsFiles dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then return []
    else do
      entries <- listDirectory dir
      children <- forM entries $ \entry -> do
        let path = dir </> entry
        isDir <- doesDirectoryExist path
        if isDir
          then collectHsFiles path
          else return [path | isHaskellFile path]
      return (concat children)
