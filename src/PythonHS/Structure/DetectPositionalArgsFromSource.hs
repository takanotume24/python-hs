module PythonHS.Structure.DetectPositionalArgsFromSource (detectPositionalArgsFromSource) where

import Language.Haskell.Exts
  ( ParseResult (..),
    parseModuleWithMode,
    defaultParseMode,
    parseFilename,
  )
import PythonHS.Structure.DetectFromModule (detectFromModule)
import PythonHS.Structure.PositionalArgViolation (PositionalArgViolation)

detectPositionalArgsFromSource :: FilePath -> String -> IO [PositionalArgViolation]
detectPositionalArgsFromSource path src =
  case parseModuleWithMode defaultParseMode { parseFilename = path } src of
    ParseOk m -> return (detectFromModule path m)
    ParseFailed _ _ -> return []
