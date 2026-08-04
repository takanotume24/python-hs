module PythonHS.VM.CompileDecoratorApplications (compileDecoratorApplications) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.VM.CompileDecoratorApplicationsConfig (CompileDecoratorApplicationsConfig (..))
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.DecoratorApplyResult (DecoratorApplyResult (..))
import PythonHS.VM.ExprPosition (exprPosition)
import PythonHS.VM.Instruction (Instruction (..))
import PythonHS.VM.ToCallArg (toCallArg)
import PythonHS.VM.ToCallArgInput (ToCallArgInput (..))

compileDecoratorApplications :: CompileDecoratorApplicationsConfig -> Either String CompileExprResult
compileDecoratorApplications config = compileAt baseIndex (reverse decorators)
  where
    compileExpr = compileDecoratorApplicationsCompileExpr config
    baseIndex = compileDecoratorApplicationsBaseIndex config
    targetName = compileDecoratorApplicationsTargetName config
    decorators = compileDecoratorApplicationsDecorators config
    compileAt :: Int -> [Expr] -> Either String CompileExprResult
    compileAt idx remaining =
      case remaining of
        [] -> Right (CompileExprResult {compileExprResultCode = [], compileExprResultEndIndex = idx})
        decoratorExpr : rest -> do
          let pos = exprPosition decoratorExpr
          applyResult <-
            case decoratorExpr of
              IdentifierExpr {identifierExprName = decoratorName} ->
                Right DecoratorApplyResult {decoratorApplyResultCode = [CallFunction {callFunctionName = decoratorName, callFunctionArgs = [toCallArg ToCallArgInput {toCallArgInputCode = [LoadName {loadNameName = targetName, loadNamePos = pos}], toCallArgInputKeyword = Nothing, toCallArgInputPos = pos}], callFunctionPos = pos}, StoreName {storeNameName = targetName}], decoratorApplyResultNextIndex = idx + 2}
              _ -> do
                decoratorResult <- compileExpr idx decoratorExpr
                Right DecoratorApplyResult {decoratorApplyResultCode = compileExprResultCode decoratorResult ++ [CallValueFunction {callValueFunctionArgs = [toCallArg ToCallArgInput {toCallArgInputCode = [LoadName {loadNameName = targetName, loadNamePos = pos}], toCallArgInputKeyword = Nothing, toCallArgInputPos = pos}], callValueFunctionPos = pos}, StoreName {storeNameName = targetName}], decoratorApplyResultNextIndex = compileExprResultEndIndex decoratorResult + 2}
          restResult <- compileAt (decoratorApplyResultNextIndex applyResult) rest
          Right (CompileExprResult {compileExprResultCode = decoratorApplyResultCode applyResult ++ compileExprResultCode restResult, compileExprResultEndIndex = compileExprResultEndIndex restResult})
