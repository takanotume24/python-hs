module PythonHS.VM.CompileCallArgsAt (compileCallArgsAt) where

import PythonHS.AST.Expr (Expr (KeywordArgExpr, KwStarArgExpr, StarArgExpr))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileCallArgsAtConfig (CompileCallArgsAtConfig (..))
import PythonHS.VM.ExprPosition (exprPosition)
import PythonHS.VM.Instruction (Instruction)

compileCallArgsAt :: CompileCallArgsAtConfig -> Either String [([Instruction], Maybe String, Position)]
compileCallArgsAt config =
  let compileExpr = compileCallArgsAtCompileExpr config
      args = compileCallArgsAtArgs config
   in case args of
        [] -> Right []
        argExpr : restArgs ->
          case argExpr of
            KeywordArgExpr argName valueExpr argPos -> do
              (argCode, _) <- compileExpr 0 valueExpr
              restCompiledArgs <- compileCallArgsAt config {compileCallArgsAtCompileExpr = compileExpr, compileCallArgsAtArgs = restArgs}
              pure ((argCode, Just argName, argPos) : restCompiledArgs)
            StarArgExpr valueExpr argPos -> do
              (argCode, _) <- compileExpr 0 valueExpr
              restCompiledArgs <- compileCallArgsAt config {compileCallArgsAtCompileExpr = compileExpr, compileCallArgsAtArgs = restArgs}
              pure ((argCode, Just starArgMarker, argPos) : restCompiledArgs)
            KwStarArgExpr valueExpr argPos -> do
              (argCode, _) <- compileExpr 0 valueExpr
              restCompiledArgs <- compileCallArgsAt config {compileCallArgsAtCompileExpr = compileExpr, compileCallArgsAtArgs = restArgs}
              pure ((argCode, Just kwStarArgMarker, argPos) : restCompiledArgs)
            _ -> do
              (argCode, _) <- compileExpr 0 argExpr
              restCompiledArgs <- compileCallArgsAt config {compileCallArgsAtCompileExpr = compileExpr, compileCallArgsAtArgs = restArgs}
              pure ((argCode, Nothing, exprPosition argExpr) : restCompiledArgs)
  where
    starArgMarker = "__python_hs_star_arg__"
    kwStarArgMarker = "__python_hs_kwstar_arg__"
