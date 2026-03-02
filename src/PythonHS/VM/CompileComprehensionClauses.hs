module PythonHS.VM.CompileComprehensionClauses (compileComprehensionClauses) where

import PythonHS.AST.Expr (Expr)
import PythonHS.VM.Instruction (Instruction (ReturnTop))

compileComprehensionClauses ::
  (Int -> Expr -> Either String ([Instruction], Int)) ->
  [(String, Expr, Maybe Expr)] ->
  Either String [(String, [Instruction], Maybe [Instruction])]
compileComprehensionClauses compileExprAt clauses =
  case clauses of
    [] -> Right []
    (name, iterExpr, maybeCondExpr) : rest -> do
      (iterCode, _) <- compileExprAt 0 iterExpr
      maybeCondCode <-
        case maybeCondExpr of
          Nothing -> Right Nothing
          Just condExpr -> do
            (condCode, _) <- compileExprAt 0 condExpr
            Right (Just (condCode ++ [ReturnTop]))
      restClauses <- compileComprehensionClauses compileExprAt rest
      Right ((name, iterCode ++ [ReturnTop], maybeCondCode) : restClauses)
