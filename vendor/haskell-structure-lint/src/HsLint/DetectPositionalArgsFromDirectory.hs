module HsLint.DetectPositionalArgsFromDirectory (detectPositionalArgsFromDirectory) where

import Data.List (isInfixOf)
import HsLint.CollectHsFiles (collectHsFiles)
import HsLint.DetectFromModule (detectFromModule)
import HsLint.DetectFromModuleConfig (DetectFromModuleConfig (..))
import HsLint.DetectModuleConfig (DetectModuleConfig (..))
import HsLint.DetectPositionalArgsFromDirectoryConfig (DetectPositionalArgsFromDirectoryConfig (..))
import HsLint.Ghc2024LikeExtensions (ghc2024LikeExtensions)
import HsLint.PositionalArgViolation (PositionalArgViolation)
import Language.Haskell.Exts
  ( Extension (..),
    ParseMode (..),
    ParseResult (..),
    defaultParseMode,
    parseFilename,
    parseModuleWithMode,
  )

detectPositionalArgsFromDirectory :: DetectPositionalArgsFromDirectoryConfig -> IO [PositionalArgViolation]
detectPositionalArgsFromDirectory config = do
  hsFiles <- collectHsFiles (detectPositionalArgsFromDirectoryDir config)
  let filteredFiles = filter (not . matchesAnyExclude) hsFiles
  concat <$> mapM goFile filteredFiles
  where
    excludes = detectPositionalArgsFromDirectoryExcludes config
    skipFunctionPaths = detectPositionalArgsFromDirectorySkipFunctionPaths config

    parseModeFor path = defaultParseMode {parseFilename = path, extensions = map EnableExtension ghc2024LikeExtensions}

    goFile path = do
      src <- readFile path
      case parseModuleWithMode (parseModeFor path) src of
        ParseOk m ->
          pure
            ( detectFromModule
                DetectFromModuleConfig
                  { detectFromModuleModuleConfig = DetectModuleConfig {moduleFilePath = path, moduleAst = m},
                    detectFromModuleSkipFunctionPaths = skipFunctionPaths
                  }
            )
        ParseFailed _ _ -> pure []

    matchesAnyExclude path = any (`isInfixOf` path) excludes
