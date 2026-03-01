module PythonHS.VM.CompileCallArgsAt (compileCallArgsAt) where

import PythonHS.AST.Expr (Expr (KeywordArgExpr))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.ExprPosition (exprPosition)
import PythonHS.VM.Instruction (Instruction)

compileCallArgsAt :: (Int -> Expr -> Either String ([Instruction], Int)) -> Int -> [Expr] -> Either String ([Instruction], [(Maybe String, Position)], Int)
compileCallArgsAt compileExprAt baseIndex args =
  case args of
    [] -> Right ([], [], baseIndex)
    argExpr : restArgs ->
      case argExpr of
        KeywordArgExpr argName valueExpr argPos -> do
          (argCode, argEnd) <- compileExprAt baseIndex valueExpr
          (restCode, restKinds, restEnd) <- compileCallArgsAt compileExprAt argEnd restArgs
          pure (argCode ++ restCode, (Just argName, argPos) : restKinds, restEnd)
        _ -> do
          (argCode, argEnd) <- compileExprAt baseIndex argExpr
          (restCode, restKinds, restEnd) <- compileCallArgsAt compileExprAt argEnd restArgs
          pure (argCode ++ restCode, (Nothing, exprPosition argExpr) : restKinds, restEnd)
