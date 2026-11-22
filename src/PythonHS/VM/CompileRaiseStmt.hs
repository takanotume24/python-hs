module PythonHS.VM.CompileRaiseStmt (compileRaiseStmt) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.CompileRaiseStmtConfig (CompileRaiseStmtConfig (..))
import PythonHS.VM.Instruction (Instruction (..))
import PythonHS.VM.IsBuiltinExceptionClass (isBuiltinExceptionClass)

compileRaiseStmt :: CompileRaiseStmtConfig -> Either String CompileExprResult
compileRaiseStmt config =
  case maybeExpr of
    Nothing ->
      pure (CompileExprResult {compileExprResultCode = [RaisePendingException], compileExprResultEndIndex = baseIndex + 1})
    Just expr ->
      case expr of
        IdentifierExpr {identifierExprName = className}
          | isBuiltinExceptionClass className ->
              pure
                ( CompileExprResult
                    { compileExprResultCode =
                        [ PushConst {pushConstValue = ExceptionValue {exceptionValueClass = className, exceptionValueMessage = ""}},
                          RaiseTop {raiseTopPos = pos}
                        ],
                      compileExprResultEndIndex = baseIndex + 2
                    }
                )
        _ -> do
          exprResult <- compileExpr baseIndex expr
          let code = compileExprResultCode exprResult ++ [RaiseTop {raiseTopPos = pos}]
          pure (CompileExprResult {compileExprResultCode = code, compileExprResultEndIndex = compileExprResultEndIndex exprResult + 1})
  where
    compileExpr = compileRaiseStmtCompileExpr config
    baseIndex = compileRaiseStmtBaseIndex config
    maybeExpr = compileRaiseStmtMaybeExpr config
    pos = compileRaiseStmtPos config
