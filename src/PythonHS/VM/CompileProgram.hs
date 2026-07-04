module PythonHS.VM.CompileProgram (compileProgram) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AddOperator, DivideOperator, FloorDivideOperator, ModuloOperator, MultiplyOperator, SubtractOperator))
import PythonHS.AST.Program (Program (..))
import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.VM.CompileClassDefStmt (compileClassDefStmt)
import PythonHS.VM.CompileCompoundAssign (compileCompoundAssign)
import PythonHS.VM.CompileDecoratedStmt (compileDecoratedStmt)
import PythonHS.VM.CompileDefaults (compileDefaults)
import PythonHS.VM.CompileExprAt (compileExprAt)
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.CompileFunctionDefStmt (compileFunctionDefStmt)
import PythonHS.VM.CompileImportStmt (compileImportStmt)
import PythonHS.VM.CompileMatch (compileMatch)
import PythonHS.VM.CompileTryExcept (compileTryExcept)
import PythonHS.VM.CompileWithStmt (compileWithStmt)
import PythonHS.VM.CompileYieldCollectStmt (compileYieldCollectStmt)
import PythonHS.VM.ExprPosition (exprPosition)
import PythonHS.VM.Instruction (Instruction (..))
import PythonHS.VM.StmtPosition (stmtPosition)

compileProgram :: Program -> Either String [Instruction]
compileProgram Program {programStmts = stmts} = do
  compiledResult <- compileStatements 0 False Nothing stmts
  let compiled = compileExprResultCode compiledResult
      nextIndex = compileExprResultEndIndex compiledResult
  if nextIndex == length compiled
    then pure (compiled ++ [Halt])
    else Left "VM compile error: internal instruction index mismatch"
  where
    compileStatements baseIndex inFunction maybeLoop items =
      case items of
        [] -> Right (CompileExprResult {compileExprResultCode = [], compileExprResultEndIndex = baseIndex})
        stmt : rest -> do
          stmtResult <- compileStmt baseIndex inFunction maybeLoop stmt
          restResult <- compileStatements (compileExprResultEndIndex stmtResult) inFunction maybeLoop rest
          pure (CompileExprResult {compileExprResultCode = compileExprResultCode stmtResult ++ compileExprResultCode restResult, compileExprResultEndIndex = compileExprResultEndIndex restResult})

    compileStmt baseIndex inFunction maybeLoop stmt =
      case stmt of
        PassStmt {} -> Right (CompileExprResult {compileExprResultCode = [], compileExprResultEndIndex = baseIndex})
        DecoratedStmt {decoratedStmtDecorators = decorators, decoratedStmtTarget = targetStmt} -> compileDecoratedStmt compileStmt (compileClassDefStmt compileDefaults compileStatements compileExprAt) compileExprAt baseIndex inFunction maybeLoop decorators targetStmt
        GlobalStmt {globalStmtName = name} -> Right (CompileExprResult {compileExprResultCode = [DeclareGlobal {declareGlobalName = name}], compileExprResultEndIndex = baseIndex + 1})
        RaiseStmt {raiseStmtExpr = expr, raiseStmtPos = pos} -> do
          exprResult <- compileExprAt baseIndex expr
          let code = compileExprResultCode exprResult ++ [RaiseTop {raiseTopPos = pos}]
          pure (CompileExprResult {compileExprResultCode = code, compileExprResultEndIndex = compileExprResultEndIndex exprResult + 1})
        TryExceptStmt {tryExceptStmtTryBody = tryStmts, tryExceptStmtExceptSuites = exceptStmts, tryExceptStmtFinallyBody = maybeFinally} -> compileTryExcept compileStatements baseIndex inFunction maybeLoop tryStmts exceptStmts maybeFinally
        MatchStmt {matchStmtSubject = subjectExpr, matchStmtCases = matchCases} -> compileMatch compileExprAt compileStatements baseIndex inFunction maybeLoop subjectExpr matchCases
        ImportStmt {} -> compileImportStmt baseIndex stmt
        FromImportStmt {} -> compileImportStmt baseIndex stmt
        AssignStmt {assignStmtName = name, assignStmtValue = expr} -> do
          exprResult <- compileExprAt baseIndex expr
          let code = compileExprResultCode exprResult ++ [StoreName {storeNameName = name}]
          pure (CompileExprResult {compileExprResultCode = code, compileExprResultEndIndex = compileExprResultEndIndex exprResult + 1})
        AssignUnpackStmt {assignUnpackStmtNames = names, assignUnpackStmtValue = expr, assignUnpackStmtPos = pos} -> do
          exprResult <- compileExprAt baseIndex expr
          let code = compileExprResultCode exprResult ++ [UnpackToNames {unpackToNamesNames = names, unpackToNamesPos = pos}]
          pure (CompileExprResult {compileExprResultCode = code, compileExprResultEndIndex = compileExprResultEndIndex exprResult + 1})
        AnnAssignStmt {annAssignStmtName = name, annAssignStmtValue = maybeExpr} -> case maybeExpr of
          Nothing -> Right (CompileExprResult {compileExprResultCode = [], compileExprResultEndIndex = baseIndex})
          Just expr -> do
            exprResult <- compileExprAt baseIndex expr
            let code = compileExprResultCode exprResult ++ [StoreName {storeNameName = name}]
            pure (CompileExprResult {compileExprResultCode = code, compileExprResultEndIndex = compileExprResultEndIndex exprResult + 1})
        AddAssignStmt {addAssignStmtName = name, addAssignStmtValue = expr, addAssignStmtPos = pos} -> compileCompoundAssign compileExprAt baseIndex name expr pos AddOperator
        SubAssignStmt {subAssignStmtName = name, subAssignStmtValue = expr, subAssignStmtPos = pos} -> compileCompoundAssign compileExprAt baseIndex name expr pos SubtractOperator
        MulAssignStmt {mulAssignStmtName = name, mulAssignStmtValue = expr, mulAssignStmtPos = pos} -> compileCompoundAssign compileExprAt baseIndex name expr pos MultiplyOperator
        DivAssignStmt {divAssignStmtName = name, divAssignStmtValue = expr, divAssignStmtPos = pos} -> compileCompoundAssign compileExprAt baseIndex name expr pos DivideOperator
        ModAssignStmt {modAssignStmtName = name, modAssignStmtValue = expr, modAssignStmtPos = pos} -> compileCompoundAssign compileExprAt baseIndex name expr pos ModuloOperator
        FloorDivAssignStmt {floorDivAssignStmtName = name, floorDivAssignStmtValue = expr, floorDivAssignStmtPos = pos} -> compileCompoundAssign compileExprAt baseIndex name expr pos FloorDivideOperator
        PrintStmt {printStmtValue = expr} -> do
          exprResult <- compileExprAt baseIndex expr
          let code = compileExprResultCode exprResult ++ [PrintTop]
          pure (CompileExprResult {compileExprResultCode = code, compileExprResultEndIndex = compileExprResultEndIndex exprResult + 1})
        YieldStmt {yieldStmtValue = expr, yieldStmtPos = pos} -> if inFunction then compileYieldCollectStmt compileExprAt baseIndex "append" expr pos else Left ("VM compile error: unsupported statement at " ++ showPos (stmtPosition stmt))
        YieldFromStmt {yieldFromStmtValue = expr, yieldFromStmtPos = pos} -> if inFunction then compileYieldCollectStmt compileExprAt baseIndex "extend" expr pos else Left ("VM compile error: unsupported statement at " ++ showPos (stmtPosition stmt))
        IfStmt {ifStmtCond = cond, ifStmtThen = thenStmts, ifStmtElse = maybeElseStmts} -> do
          condResult <- compileExprAt baseIndex cond
          let jumpIfFalseIndex = compileExprResultEndIndex condResult
          let thenStartIndex = jumpIfFalseIndex + 1
          thenResult <- compileStatements thenStartIndex inFunction maybeLoop thenStmts
          case maybeElseStmts of
            Nothing -> do
              let jumpFalseTarget = compileExprResultEndIndex thenResult
              let code = compileExprResultCode condResult ++ [JumpIfFalse {jumpIfFalseTarget = jumpFalseTarget}] ++ compileExprResultCode thenResult
              pure (CompileExprResult {compileExprResultCode = code, compileExprResultEndIndex = compileExprResultEndIndex thenResult})
            Just elseStmts -> do
              let jumpOverElseIndex = compileExprResultEndIndex thenResult
              let elseStartIndex = jumpOverElseIndex + 1
              elseResult <- compileStatements elseStartIndex inFunction maybeLoop elseStmts
              let code = compileExprResultCode condResult ++ [JumpIfFalse {jumpIfFalseTarget = elseStartIndex}] ++ compileExprResultCode thenResult ++ [Jump {jumpTarget = compileExprResultEndIndex elseResult}] ++ compileExprResultCode elseResult
              pure (CompileExprResult {compileExprResultCode = code, compileExprResultEndIndex = compileExprResultEndIndex elseResult})
        WhileStmt {whileStmtCond = cond, whileStmtBody = body, whileStmtPos = whilePos} -> do
          condResult <- compileExprAt baseIndex cond
          let jumpIfFalseIndex = compileExprResultEndIndex condResult
          let bodyStartIndex = jumpIfFalseIndex + 1
          let guardIndex = bodyStartIndex
          let firstBodyStmtIndex = guardIndex + 1
          let provisionalLoopContext = Just (0, baseIndex)
          provisionalBodyResult <- compileStatements firstBodyStmtIndex inFunction provisionalLoopContext body
          let loopEndIndex = compileExprResultEndIndex provisionalBodyResult + 1
          let loopContext = Just (loopEndIndex, baseIndex)
          bodyResult <- compileStatements firstBodyStmtIndex inFunction loopContext body
          let code = compileExprResultCode condResult ++ [JumpIfFalse {jumpIfFalseTarget = loopEndIndex}, LoopGuard {loopGuardPos = whilePos}] ++ compileExprResultCode bodyResult ++ [Jump {jumpTarget = baseIndex}]
          pure (CompileExprResult {compileExprResultCode = code, compileExprResultEndIndex = loopEndIndex})
        ForStmt {forStmtVar = name, forStmtIter = iterExpr, forStmtBody = body, forStmtPos = forPos} -> do
          iterResult <- compileExprAt baseIndex iterExpr
          let setupIndex = compileExprResultEndIndex iterResult
          let nextIndex = setupIndex + 1
          let guardIndex = nextIndex + 1
          let bodyStartIndex = guardIndex + 1
          let provisionalLoopContext = Just (0, nextIndex)
          provisionalBodyResult <- compileStatements bodyStartIndex inFunction provisionalLoopContext body
          let loopEndIndex = compileExprResultEndIndex provisionalBodyResult + 1
          let loopContext = Just (loopEndIndex, nextIndex)
          bodyResult <- compileStatements bodyStartIndex inFunction loopContext body
          let iterPos = exprPosition iterExpr
          let code = compileExprResultCode iterResult ++ [ForSetup {forSetupNextIndex = nextIndex, forSetupPos = iterPos}, ForNext {forNextName = name, forNextLoopEndIndex = loopEndIndex, forNextPos = iterPos}, LoopGuard {loopGuardPos = forPos}] ++ compileExprResultCode bodyResult ++ [Jump {jumpTarget = nextIndex}]
          pure (CompileExprResult {compileExprResultCode = code, compileExprResultEndIndex = loopEndIndex})
        ClassDefStmt {classDefStmtName = className, classDefStmtBase = maybeBase, classDefStmtBody = body} -> compileClassDefStmt compileDefaults compileStatements compileExprAt baseIndex className maybeBase body Nothing
        FunctionDefStmt {functionDefStmtName = name, functionDefStmtParams = params, functionDefStmtBody = body, functionDefStmtPos = posDef} -> fmap (\(functionCode, _) -> CompileExprResult {compileExprResultCode = [DefineFunction {defineFunctionName = name, defineFunctionParams = params, defineFunctionDefaultCodes = [], defineFunctionCode = functionCode}], compileExprResultEndIndex = baseIndex + 1}) (compileFunctionDefStmt compileStatements compileExprAt posDef [] body)
        FunctionDefDefaultsStmt {functionDefDefaultsStmtName = name, functionDefDefaultsStmtParams = params, functionDefDefaultsStmtDefaults = defaults, functionDefDefaultsStmtBody = body, functionDefDefaultsStmtPos = posDef} -> fmap (\(functionCode, defaultCodes) -> CompileExprResult {compileExprResultCode = [DefineFunction {defineFunctionName = name, defineFunctionParams = params, defineFunctionDefaultCodes = defaultCodes, defineFunctionCode = functionCode}], compileExprResultEndIndex = baseIndex + 1}) (compileFunctionDefStmt compileStatements compileExprAt posDef defaults body)
        ReturnStmt {returnStmtValue = expr} ->
          if inFunction
            then do
              exprResult <- compileExprAt baseIndex expr
              let code = compileExprResultCode exprResult ++ [ReturnTop]
              pure (CompileExprResult {compileExprResultCode = code, compileExprResultEndIndex = compileExprResultEndIndex exprResult + 1})
            else Left ("VM compile error: unsupported statement at " ++ showPos (stmtPosition stmt))
        BreakStmt {breakStmtPos = pos} -> case maybeLoop of
          Just (breakTarget, _) -> Right (CompileExprResult {compileExprResultCode = [Jump {jumpTarget = breakTarget}], compileExprResultEndIndex = baseIndex + 1})
          Nothing -> Left ("Break outside loop at " ++ showPos pos)
        ContinueStmt {continueStmtPos = pos} -> case maybeLoop of
          Just (_, continueTarget) -> Right (CompileExprResult {compileExprResultCode = [Jump {jumpTarget = continueTarget}], compileExprResultEndIndex = baseIndex + 1})
          Nothing -> Left ("Continue outside loop at " ++ showPos pos)
        WithStmt {withStmtContextManager = contextManager, withStmtVarName = maybeVarName, withStmtBody = body, withStmtPos = withPos} -> compileWithStmt baseIndex inFunction maybeLoop contextManager maybeVarName body withPos compileStatements
