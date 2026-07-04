module PythonHS.Structure.DetectPositionalArgs
  ( detectPositionalArgs,
    detectPositionalArgsFromSource,
  )
where

import PythonHS.Structure.DetectPositionalArgsFromSource (detectPositionalArgsFromSource)
import PythonHS.Structure.DetectSourceConfig (DetectSourceConfig (..))
import PythonHS.Structure.PositionalArgViolation (PositionalArgViolation)

detectPositionalArgs :: FilePath -> IO [PositionalArgViolation]
detectPositionalArgs path = do
  src <- readFile path
  detectPositionalArgsFromSource (DetectSourceConfig {sourceFilePath = path, sourceContent = src})
