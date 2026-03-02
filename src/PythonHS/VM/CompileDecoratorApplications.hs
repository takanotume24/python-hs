module PythonHS.VM.CompileDecoratorApplications (compileDecoratorApplications) where

import PythonHS.AST.Expr (Expr (IdentifierExpr), Expr)
import PythonHS.VM.ExprPosition (exprPosition)
import PythonHS.VM.Instruction (Instruction (CallFunction, CallValueFunction, LoadName, StoreName), Instruction)

compileDecoratorApplications ::
  (Int -> Expr -> Either String ([Instruction], Int)) ->
  Int ->
  String ->
  [Expr] ->
  Either String ([Instruction], Int)
compileDecoratorApplications compileExprAt baseIndex targetName decorators =
  compileAt baseIndex (reverse decorators)
  where
    compileAt idx remaining =
      case remaining of
        [] -> Right ([], idx)
        decoratorExpr : rest -> do
          let pos = exprPosition decoratorExpr
          (applyCode, afterApply) <-
            case decoratorExpr of
              IdentifierExpr decoratorName _ ->
                Right ([CallFunction decoratorName [([LoadName targetName pos], Nothing, pos)] pos, StoreName targetName], idx + 2)
              _ -> do
                (decoratorCode, decoratorEnd) <- compileExprAt idx decoratorExpr
                Right (decoratorCode ++ [CallValueFunction [([LoadName targetName pos], Nothing, pos)] pos, StoreName targetName], decoratorEnd + 2)
          (restCode, afterRest) <- compileAt afterApply rest
          Right (applyCode ++ restCode, afterRest)
