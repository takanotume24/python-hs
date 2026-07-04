module PythonHS.VM.CompileDecoratedStmt (compileDecoratedStmt) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.VM.CompileDecoratedStmtConfig (CompileDecoratedStmtConfig (..))
import PythonHS.VM.CompileDecoratorApplications (compileDecoratorApplications)
import PythonHS.VM.CompileDecoratorApplicationsConfig (CompileDecoratorApplicationsConfig (..))
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.StmtPosition (stmtPosition)

compileDecoratedStmt :: CompileDecoratedStmtConfig -> Either String CompileExprResult
compileDecoratedStmt config = case parseDataclassConfig decorators of
  Right (Just dataclassConfig) ->
    case targetStmt of
      ClassDefStmt {classDefStmtName = className, classDefStmtBase = maybeBase, classDefStmtBody = body} ->
        compileDataclassClass baseIndex className maybeBase body (Just dataclassConfig)
      _ -> compileDefault
  Right Nothing -> compileDefault
  Left err -> Left err
  where
    compileStmt = compileDecoratedStmtCompileStmt config
    compileDataclassClass = compileDecoratedStmtCompileDataclassClass config
    compileExpr = compileDecoratedStmtCompileExpr config
    baseIndex = compileDecoratedStmtBaseIndex config
    inFunction = compileDecoratedStmtInFunction config
    maybeLoop = compileDecoratedStmtMaybeLoop config
    decorators = compileDecoratedStmtDecorators config
    targetStmt = compileDecoratedStmtTargetStmt config
    compileDefault = do
      targetResult <- compileStmt baseIndex inFunction maybeLoop targetStmt
      targetName <- decoratedTargetName targetStmt
      decoratorResult <- compileDecoratorApplications CompileDecoratorApplicationsConfig {compileDecoratorApplicationsCompileExpr = compileExpr, compileDecoratorApplicationsBaseIndex = compileExprResultEndIndex targetResult, compileDecoratorApplicationsTargetName = targetName, compileDecoratorApplicationsDecorators = decorators}
      pure (CompileExprResult {compileExprResultCode = compileExprResultCode targetResult ++ compileExprResultCode decoratorResult, compileExprResultEndIndex = compileExprResultEndIndex decoratorResult})

    decoratedTargetName stmt =
      case stmt of
        FunctionDefStmt {functionDefStmtName = name} -> Right name
        FunctionDefDefaultsStmt {functionDefDefaultsStmtName = name} -> Right name
        ClassDefStmt {classDefStmtName = name} -> Right name
        _ -> Left ("VM compile error: unsupported decorator target at " ++ showPos (stmtPosition stmt))

    parseDataclassConfig exprs =
      case exprs of
        [IdentifierExpr {identifierExprName = "dataclass"}] -> Right (Just (False, False))
        [CallExpr {callExprName = "dataclass", callExprArgs = args}] -> parseDataclassArgs args
        _ -> Right Nothing

    parseDataclassArgs args = parseArgs args (False, False)

    parseArgs args (frozenNow, orderNow) =
      case args of
        [] -> Right (Just (frozenNow, orderNow))
        KeywordArgExpr {keywordArgExprName = "frozen", keywordArgExprValue = IntegerExpr {integerExprValue = n}} : rest ->
          parseArgs rest (n /= 0, orderNow)
        KeywordArgExpr {keywordArgExprName = "order", keywordArgExprValue = IntegerExpr {integerExprValue = n}} : rest ->
          parseArgs rest (frozenNow, n /= 0)
        KeywordArgExpr {keywordArgExprName = name} : _ ->
          Left ("VM compile error: unsupported dataclass option " ++ name ++ " at " ++ showPos (stmtPosition targetStmt))
        _ -> Left ("VM compile error: unsupported dataclass decorator arguments at " ++ showPos (stmtPosition targetStmt))
