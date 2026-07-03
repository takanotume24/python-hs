module PythonHS.Structure.DetectPositionalArgsFromDirectory (detectPositionalArgsFromDirectory) where

import Data.List (isInfixOf)
import PythonHS.Structure.CollectHsFiles (collectHsFiles)
import PythonHS.Structure.DetectPositionalArgsFromDirectoryConfig (DetectPositionalArgsFromDirectoryConfig (..))
import PythonHS.Structure.DetectPositionalArgsFromSource (detectPositionalArgsFromSource)
import PythonHS.Structure.DetectSourceConfig (DetectSourceConfig (..))
import PythonHS.Structure.PositionalArgViolation (PositionalArgViolation)

detectPositionalArgsFromDirectory :: DetectPositionalArgsFromDirectoryConfig -> IO [PositionalArgViolation]
detectPositionalArgsFromDirectory config = do
  let dir = detectPositionalArgsFromDirectoryDir config
      excludes = detectPositionalArgsFromDirectoryExcludes config
  hsFiles <- collectHsFiles dir
  let filteredFiles = filter (not . matchesAnyExclude excludes) hsFiles
  fmap concat $ mapM goFile filteredFiles
  where
    goFile path = do
      src <- readFile path
      detectPositionalArgsFromSource (DetectSourceConfig path src)

    matchesAnyExclude patterns path = any (\p -> p `isInfixOf` path) patterns
