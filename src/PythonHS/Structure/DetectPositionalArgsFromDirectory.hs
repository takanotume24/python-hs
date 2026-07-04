module PythonHS.Structure.DetectPositionalArgsFromDirectory (detectPositionalArgsFromDirectory) where

import Data.List (isInfixOf)
import Language.Haskell.Exts
  ( ParseResult (..),
    parseModuleWithMode,
    defaultParseMode,
    parseFilename,
  )
import PythonHS.Structure.CollectHsFiles (collectHsFiles)
import PythonHS.Structure.CollectRecordConNames (collectRecordConNames)
import PythonHS.Structure.DetectFromModule (detectFromModule)
import PythonHS.Structure.DetectModuleConfig (DetectModuleConfig (..))
import PythonHS.Structure.DetectPositionalArgsFromDirectoryConfig (DetectPositionalArgsFromDirectoryConfig (..))
import PythonHS.Structure.PositionalArgViolation (PositionalArgViolation)

detectPositionalArgsFromDirectory :: DetectPositionalArgsFromDirectoryConfig -> IO [PositionalArgViolation]
detectPositionalArgsFromDirectory config = do
  let dir = detectPositionalArgsFromDirectoryDir config
      excludes = detectPositionalArgsFromDirectoryExcludes config
  hsFiles <- collectHsFiles dir
  let filteredFiles = filter (not . matchesAnyExclude excludes) hsFiles
  allRecordConNames <- fmap concat $ mapM collectNamesFromFile filteredFiles
  fmap concat $ mapM (goFile allRecordConNames) filteredFiles
  where
    collectNamesFromFile path = do
      src <- readFile path
      case parseModuleWithMode defaultParseMode { parseFilename = path } src of
        ParseOk m -> pure (collectRecordConNames m)
        ParseFailed _ _ -> pure []

    goFile allRecordConNames path = do
      src <- readFile path
      case parseModuleWithMode defaultParseMode { parseFilename = path } src of
        ParseOk m -> pure (detectFromModule allRecordConNames (DetectModuleConfig path m))
        ParseFailed _ _ -> pure []

    matchesAnyExclude patterns path = any (\p -> p `isInfixOf` path) patterns
