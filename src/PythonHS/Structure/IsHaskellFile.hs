module PythonHS.Structure.IsHaskellFile (isHaskellFile) where

import System.FilePath (takeExtension)

isHaskellFile :: FilePath -> Bool
isHaskellFile path = takeExtension path == ".hs"
