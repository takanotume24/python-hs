module PythonHS.Structure.DetectPositionalArgsFromSource (detectPositionalArgsFromSource) where

import Language.Haskell.Exts
  ( Extension (..),
    ParseMode (..),
    ParseResult (..),
    defaultParseMode,
    parseFilename,
    parseModuleWithMode,
  )
import PythonHS.Structure.CollectRecordConNames (collectRecordConNames)
import PythonHS.Structure.DetectFromModule (detectFromModule)
import PythonHS.Structure.DetectFromModuleConfig (DetectFromModuleConfig (..))
import PythonHS.Structure.DetectModuleConfig (DetectModuleConfig (..))
import PythonHS.Structure.DetectSourceConfig (DetectSourceConfig (..))
import PythonHS.Structure.Ghc2024LikeExtensions (ghc2024LikeExtensions)
import PythonHS.Structure.PositionalArgViolation (PositionalArgViolation)

detectPositionalArgsFromSource :: DetectSourceConfig -> IO [PositionalArgViolation]
detectPositionalArgsFromSource config =
  let path = sourceFilePath config
      src = sourceContent config
      parseMode = defaultParseMode {parseFilename = path, extensions = map EnableExtension ghc2024LikeExtensions}
   in case parseModuleWithMode parseMode src of
        ParseOk m ->
          let recordConNames = collectRecordConNames m
           in return (detectFromModule (DetectFromModuleConfig {detectFromModuleRecordConNames = recordConNames, detectFromModuleModuleConfig = DetectModuleConfig {moduleFilePath = path, moduleAst = m}}))
        ParseFailed _ _ -> return []
