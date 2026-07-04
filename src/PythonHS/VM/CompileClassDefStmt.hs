module PythonHS.VM.CompileClassDefStmt (compileClassDefStmt) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.VM.CompileClassDefStmtConfig (CompileClassDefStmtConfig (..))
import PythonHS.VM.CompileClassStmt (compileClassStmt)
import PythonHS.VM.CompileClassStmtConfig (CompileClassStmtConfig (..))
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.Instruction (Instruction)

compileClassDefStmt :: CompileClassDefStmtConfig -> Either String CompileExprResult
compileClassDefStmt config =
  compileClassStmt
    CompileClassStmtConfig
      { compileClassStmtCompileDefaults = compileClassDefStmtCompileDefaults config,
        compileClassStmtCompileStatements = compileClassDefStmtCompileStatements config,
        compileClassStmtCompileExpr = compileClassDefStmtCompileExpr config,
        compileClassStmtBaseIndex = compileClassDefStmtBaseIndex config,
        compileClassStmtClassName = compileClassDefStmtClassName config,
        compileClassStmtMaybeBase = compileClassDefStmtMaybeBase config,
        compileClassStmtBody = compileClassDefStmtBody config,
        compileClassStmtMaybeDataclass = compileClassDefStmtMaybeDataclass config
      }
