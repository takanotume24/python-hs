module PythonHS.VM.CompileDecoratorApplications (compileDecoratorApplications) where

import PythonHS.AST.Expr (Expr (IdentifierExpr))
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.ExprPosition (exprPosition)
import PythonHS.VM.Instruction (Instruction (CallFunction, CallValueFunction, LoadName, StoreName))

compileDecoratorApplications ::
  (Int -> Expr -> Either String CompileExprResult) ->
  Int ->
  String ->
  [Expr] ->
  Either String CompileExprResult
compileDecoratorApplications compileExprAt baseIndex targetName decorators =
  compileAt baseIndex (reverse decorators)
  where
    compileAt :: Int -> [Expr] -> Either String CompileExprResult
    compileAt idx remaining =
      case remaining of
        [] -> Right (CompileExprResult [] idx)
        decoratorExpr : rest -> do
          let pos = exprPosition decoratorExpr
          (applyCode, afterApply) <-
            case decoratorExpr of
              IdentifierExpr decoratorName _ ->
                Right ([CallFunction decoratorName [([LoadName targetName pos], Nothing, pos)] pos, StoreName targetName], idx + 2)
              _ -> do
                decoratorResult <- compileExprAt idx decoratorExpr
                Right (compileExprResultCode decoratorResult ++ [CallValueFunction [([LoadName targetName pos], Nothing, pos)] pos, StoreName targetName], compileExprResultEndIndex decoratorResult + 2)
          restResult <- compileAt afterApply rest
          Right (CompileExprResult (applyCode ++ compileExprResultCode restResult) (compileExprResultEndIndex restResult))
