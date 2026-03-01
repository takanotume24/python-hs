module PythonHS.VM.CompileCallArgsAt (compileCallArgsAt) where

import PythonHS.AST.Expr (Expr (KeywordArgExpr))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.ExprPosition (exprPosition)
import PythonHS.VM.Instruction (Instruction)

compileCallArgsAt :: (Int -> Expr -> Either String ([Instruction], Int)) -> [Expr] -> Either String [([Instruction], Maybe String, Position)]
compileCallArgsAt compileExprAt args =
  case args of
    [] -> Right []
    argExpr : restArgs ->
      case argExpr of
        KeywordArgExpr argName valueExpr argPos -> do
          (argCode, _) <- compileExprAt 0 valueExpr
          restCompiledArgs <- compileCallArgsAt compileExprAt restArgs
          pure ((argCode, Just argName, argPos) : restCompiledArgs)
        _ -> do
          (argCode, _) <- compileExprAt 0 argExpr
          restCompiledArgs <- compileCallArgsAt compileExprAt restArgs
          pure ((argCode, Nothing, exprPosition argExpr) : restCompiledArgs)
