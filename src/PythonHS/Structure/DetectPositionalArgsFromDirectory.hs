module PythonHS.Structure.DetectPositionalArgsFromDirectory (detectPositionalArgsFromDirectory) where

import Data.List (isInfixOf)
import PythonHS.Structure.CollectHsFiles (collectHsFiles)
import PythonHS.Structure.DetectPositionalArgsFromSource (detectPositionalArgsFromSource)
import PythonHS.Structure.DetectSourceConfig (DetectSourceConfig (..))
import PythonHS.Structure.PositionalArgViolation (PositionalArgViolation)

detectPositionalArgsFromDirectory :: FilePath -> [String] -> IO [PositionalArgViolation]
detectPositionalArgsFromDirectory dir excludes = do
  hsFiles <- collectHsFiles dir
  let filteredFiles = filter (not . matchesAnyExclude excludes) hsFiles
  fmap concat $ mapM goFile filteredFiles
  where
    goFile path = do
      src <- readFile path
      detectPositionalArgsFromSource (DetectSourceConfig path src)

    matchesAnyExclude patterns path = any (\p -> p `isInfixOf` path) patterns
