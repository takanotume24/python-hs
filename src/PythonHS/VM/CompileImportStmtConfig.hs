module PythonHS.VM.CompileImportStmtConfig (CompileImportStmtConfig (..)) where

import PythonHS.AST.Stmt (Stmt)
import PythonHS.VM.CompileExprResult (CompileExprResult)

data CompileImportStmtConfig = CompileImportStmtConfig
  { compileImportStmtBaseIndex :: Int,
    compileImportStmtStmt :: Stmt
  }
