module PythonHS.Structure.DetectPositionalArgsFromDirectory (detectPositionalArgsFromDirectory) where

import PythonHS.Structure.CollectHsFiles (collectHsFiles)
import PythonHS.Structure.DetectPositionalArgsFromSource (detectPositionalArgsFromSource)
import PythonHS.Structure.PositionalArgViolation (PositionalArgViolation)

detectPositionalArgsFromDirectory :: FilePath -> IO [PositionalArgViolation]
detectPositionalArgsFromDirectory dir = do
  hsFiles <- collectHsFiles dir
  fmap concat $ mapM goFile hsFiles
  where
    goFile path = do
      src <- readFile path
      detectPositionalArgsFromSource path src
