module PythonHS.VM.CompileFromStdlibImportConfig (CompileFromStdlibImportConfig (..)) where

import PythonHS.AST.FromImportEntry (FromImportEntry)
import PythonHS.Lexer.Position (Position)

data CompileFromStdlibImportConfig = CompileFromStdlibImportConfig
  { compileFromStdlibImportBaseIndex :: Int,
    compileFromStdlibImportModulePath :: [String],
    compileFromStdlibImportImportedNames :: [FromImportEntry],
    compileFromStdlibImportPos :: Position
  }
