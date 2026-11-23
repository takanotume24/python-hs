module HsLint.DetectPositionalArgs
  ( detectPositionalArgs,
    detectPositionalArgsFromSource,
  )
where

import HsLint.DetectPositionalArgsFromSource (detectPositionalArgsFromSource)
import HsLint.DetectSourceConfig (DetectSourceConfig (..))
import HsLint.PositionalArgViolation (PositionalArgViolation)
import HsLint.PositionalArgsConfig (PositionalArgsConfig (..))

detectPositionalArgs :: PositionalArgsConfig -> FilePath -> IO [PositionalArgViolation]
detectPositionalArgs config path = do
  src <- readFile path
  detectPositionalArgsFromSource
    DetectSourceConfig
      { sourceFilePath = path,
        sourceContent = src,
        sourceSkipFunctionPaths = positionalSkipFunctionPaths config
      }
