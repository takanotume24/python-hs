module HsLint.DetectPositionalArgsFromSource (detectPositionalArgsFromSource) where

import HsLint.DetectFromModule (detectFromModule)
import HsLint.DetectFromModuleConfig (DetectFromModuleConfig (..))
import HsLint.DetectModuleConfig (DetectModuleConfig (..))
import HsLint.DetectSourceConfig (DetectSourceConfig (..))
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

detectPositionalArgsFromSource :: DetectSourceConfig -> IO [PositionalArgViolation]
detectPositionalArgsFromSource config =
  let path = sourceFilePath config
      src = sourceContent config
      parseMode = defaultParseMode {parseFilename = path, extensions = map EnableExtension ghc2024LikeExtensions}
   in case parseModuleWithMode parseMode src of
        ParseOk m ->
          return
            ( detectFromModule
                DetectFromModuleConfig
                  { detectFromModuleModuleConfig = DetectModuleConfig {moduleFilePath = path, moduleAst = m},
                    detectFromModuleSkipFunctionPaths = sourceSkipFunctionPaths config
                  }
            )
        ParseFailed _ _ -> return []
