module PythonHS.VM.CompileDecoratorApplications (compileDecoratorApplications) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.ExprPosition (exprPosition)
import PythonHS.VM.Instruction (Instruction (..))

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
        [] -> Right (CompileExprResult {compileExprResultCode = [], compileExprResultEndIndex = idx})
        decoratorExpr : rest -> do
          let pos = exprPosition decoratorExpr
          (applyCode, afterApply) <-
            case decoratorExpr of
              IdentifierExpr {identifierExprName = decoratorName} ->
                Right ([CallFunction {callFunctionName = decoratorName, callFunctionArgs = [([LoadName {loadNameName = targetName, loadNamePos = pos}], Nothing, pos)], callFunctionPos = pos}, StoreName {storeNameName = targetName}], idx + 2)
              _ -> do
                decoratorResult <- compileExprAt idx decoratorExpr
                Right (compileExprResultCode decoratorResult ++ [CallValueFunction {callValueFunctionArgs = [([LoadName {loadNameName = targetName, loadNamePos = pos}], Nothing, pos)], callValueFunctionPos = pos}, StoreName {storeNameName = targetName}], compileExprResultEndIndex decoratorResult + 2)
          restResult <- compileAt afterApply rest
          Right (CompileExprResult {compileExprResultCode = applyCode ++ compileExprResultCode restResult, compileExprResultEndIndex = compileExprResultEndIndex restResult})
