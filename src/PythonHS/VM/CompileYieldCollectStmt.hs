module PythonHS.VM.CompileYieldCollectStmt (compileYieldCollectStmt) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.CompileYieldCollectStmtConfig (CompileYieldCollectStmtConfig (..))
import PythonHS.VM.Instruction (Instruction (..))

compileYieldCollectStmt :: CompileYieldCollectStmtConfig -> Either String CompileExprResult
compileYieldCollectStmt config =
  let compileExpr = compileYieldCollectStmtCompileExpr config
      baseIndex = compileYieldCollectStmtBaseIndex config
      operation = compileYieldCollectStmtOperation config
      yieldedExpr = compileYieldCollectStmtYieldedExpr config
      pos = compileYieldCollectStmtPos config
   in do
        collectResult <-
          compileExpr
            baseIndex
            (CallExpr {callExprName = operation, callExprArgs = [IdentifierExpr {identifierExprName = "__yield_acc__", identifierExprPos = pos}, yieldedExpr], callExprPos = pos})
        pure (CompileExprResult {compileExprResultCode = compileExprResultCode collectResult ++ [StoreName {storeNameName = "__yield_acc__"}], compileExprResultEndIndex = compileExprResultEndIndex collectResult + 1})
