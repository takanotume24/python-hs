module PythonHS.Structure.DetectPositionalArgsFromSource (detectPositionalArgsFromSource) where

import Language.Haskell.Exts
  ( ParseResult (..),
    parseModuleWithMode,
    defaultParseMode,
    parseFilename,
  )
import PythonHS.Structure.CollectRecordConNames (collectRecordConNames)
import PythonHS.Structure.DetectFromModule (detectFromModule)
import PythonHS.Structure.DetectModuleConfig (DetectModuleConfig (..))
import PythonHS.Structure.DetectSourceConfig (DetectSourceConfig (..))
import PythonHS.Structure.PositionalArgViolation (PositionalArgViolation)

detectPositionalArgsFromSource :: DetectSourceConfig -> IO [PositionalArgViolation]
detectPositionalArgsFromSource config =
  let path = sourceFilePath config
      src = sourceContent config
   in case parseModuleWithMode defaultParseMode { parseFilename = path } src of
        ParseOk m ->
          let recordConNames = collectRecordConNames m
           in return (detectFromModule recordConNames (DetectModuleConfig { moduleFilePath = path, moduleAst = m }))
        ParseFailed _ _ -> return []
