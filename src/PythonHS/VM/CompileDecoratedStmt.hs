module PythonHS.VM.CompileDecoratedStmt (compileDecoratedStmt) where

import PythonHS.AST.Expr (Expr (CallExpr, IdentifierExpr, IntegerExpr, KeywordArgExpr), Expr)
import PythonHS.AST.Stmt (Stmt (ClassDefStmt, FunctionDefDefaultsStmt, FunctionDefStmt), Stmt)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.VM.CompileDecoratorApplications (compileDecoratorApplications)
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.Instruction (Instruction)
import PythonHS.VM.StmtPosition (stmtPosition)

compileDecoratedStmt ::
  (Int -> Bool -> Maybe (Int, Int) -> Stmt -> Either String CompileExprResult) ->
  (Int -> String -> Maybe String -> [Stmt] -> Maybe (Bool, Bool) -> Either String CompileExprResult) ->
  (Int -> Expr -> Either String CompileExprResult) ->
  Int ->
  Bool ->
  Maybe (Int, Int) ->
  [Expr] ->
  Stmt ->
  Either String CompileExprResult
compileDecoratedStmt compileStmt compileDataclassClass compileExprAt baseIndex inFunction maybeLoop decorators targetStmt =
  case parseDataclassConfig decorators of
    Right (Just dataclassConfig) ->
      case targetStmt of
        ClassDefStmt className maybeBase body _ ->
          compileDataclassClass baseIndex className maybeBase body (Just dataclassConfig)
        _ -> compileDefault
    Right Nothing -> compileDefault
    Left err -> Left err
  where
    compileDefault = do
      targetResult <- compileStmt baseIndex inFunction maybeLoop targetStmt
      targetName <- decoratedTargetName targetStmt
      decoratorResult <- compileDecoratorApplications compileExprAt (compileExprResultEndIndex targetResult) targetName decorators
      pure (CompileExprResult (compileExprResultCode targetResult ++ compileExprResultCode decoratorResult) (compileExprResultEndIndex decoratorResult))

    decoratedTargetName stmt =
      case stmt of
        FunctionDefStmt name _ _ _ -> Right name
        FunctionDefDefaultsStmt name _ _ _ _ -> Right name
        ClassDefStmt name _ _ _ -> Right name
        _ -> Left ("VM compile error: unsupported decorator target at " ++ showPos (stmtPosition stmt))

    parseDataclassConfig exprs =
      case exprs of
        [IdentifierExpr "dataclass" _] -> Right (Just (False, False))
        [CallExpr "dataclass" args _] -> parseDataclassArgs args
        _ -> Right Nothing

    parseDataclassArgs args = parseArgs args (False, False)

    parseArgs args (frozenNow, orderNow) =
      case args of
        [] -> Right (Just (frozenNow, orderNow))
        KeywordArgExpr "frozen" (IntegerExpr n _) _ : rest ->
          parseArgs rest (n /= 0, orderNow)
        KeywordArgExpr "order" (IntegerExpr n _) _ : rest ->
          parseArgs rest (frozenNow, n /= 0)
        KeywordArgExpr name _ _ : _ ->
          Left ("VM compile error: unsupported dataclass option " ++ name ++ " at " ++ showPos (stmtPosition targetStmt))
        _ -> Left ("VM compile error: unsupported dataclass decorator arguments at " ++ showPos (stmtPosition targetStmt))
