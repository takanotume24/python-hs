module PythonHS.VM.CompileProgram (compileProgram) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AddOperator, DivideOperator, FloorDivideOperator, ModuloOperator, MultiplyOperator, SubtractOperator))
import PythonHS.AST.Program (Program (Program))
import PythonHS.AST.Stmt (Stmt (AddAssignStmt, AnnAssignStmt, AssignStmt, AssignUnpackStmt, BreakStmt, ClassDefStmt, ContinueStmt, DecoratedStmt, DivAssignStmt, FloorDivAssignStmt, ForStmt, FromImportStmt, FunctionDefDefaultsStmt, FunctionDefStmt, GlobalStmt, IfStmt, ImportStmt, MatchStmt, ModAssignStmt, MulAssignStmt, PassStmt, PrintStmt, RaiseStmt, ReturnStmt, SubAssignStmt, TryExceptStmt, WhileStmt, YieldFromStmt, YieldStmt))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.VM.CompileClassDefStmt (compileClassDefStmt)
import PythonHS.VM.CompileCompoundAssign (compileCompoundAssign)
import PythonHS.VM.CompileDefaults (compileDefaults)
import PythonHS.VM.CompileDecoratedStmt (compileDecoratedStmt)
import PythonHS.VM.CompileExprAt (compileExprAt)
import PythonHS.VM.CompileFunctionDefStmt (compileFunctionDefStmt)
import PythonHS.VM.CompileImportStmt (compileImportStmt)
import PythonHS.VM.CompileMatch (compileMatch)
import PythonHS.VM.CompileTryExcept (compileTryExcept)
import PythonHS.VM.ExprPosition (exprPosition)
import PythonHS.VM.CompileYieldCollectStmt (compileYieldCollectStmt)
import PythonHS.VM.Instruction (Instruction (DeclareGlobal, DefineFunction, ForNext, ForSetup, Halt, Jump, JumpIfFalse, LoopGuard, PrintTop, RaiseTop, ReturnTop, StoreName, UnpackToNames))
import PythonHS.VM.StmtPosition (stmtPosition)

compileProgram :: Program -> Either String [Instruction]
compileProgram (Program stmts) = do
  (compiled, nextIndex) <- compileStatements 0 False Nothing stmts
  if nextIndex == length compiled
    then pure (compiled ++ [Halt])
    else Left "VM compile error: internal instruction index mismatch"
  where
    compileStatements baseIndex inFunction maybeLoop items =
      case items of
        [] -> Right ([], baseIndex)
        stmt : rest -> do
          (stmtCode, afterStmt) <- compileStmt baseIndex inFunction maybeLoop stmt
          (restCode, afterRest) <- compileStatements afterStmt inFunction maybeLoop rest
          pure (stmtCode ++ restCode, afterRest)

    compileStmt baseIndex inFunction maybeLoop stmt =
      case stmt of
        PassStmt _ -> Right ([], baseIndex)
        DecoratedStmt decorators targetStmt _ ->
          compileDecoratedStmt
            compileStmt
            (compileClassDefStmt compileDefaults compileStatements compileExprAt)
            compileExprAt
            baseIndex
            inFunction
            maybeLoop
            decorators
            targetStmt
        GlobalStmt name _ -> Right ([DeclareGlobal name], baseIndex + 1)
        RaiseStmt expr pos -> do
          (exprCode, exprEnd) <- compileExprAt baseIndex expr
          let code = exprCode ++ [RaiseTop pos]
          pure (code, exprEnd + 1)
        TryExceptStmt tryStmts exceptStmts maybeFinally _ ->
          compileTryExcept compileStatements baseIndex inFunction maybeLoop tryStmts exceptStmts maybeFinally
        MatchStmt subjectExpr matchCases _ ->
          compileMatch compileExprAt compileStatements baseIndex inFunction maybeLoop subjectExpr matchCases
        ImportStmt _ _ ->
          compileImportStmt baseIndex stmt
        FromImportStmt _ _ _ ->
          compileImportStmt baseIndex stmt
        AssignStmt name expr _ -> do
          (exprCode, exprEnd) <- compileExprAt baseIndex expr
          let code = exprCode ++ [StoreName name]
          pure (code, exprEnd + 1)
        AssignUnpackStmt names expr pos -> do
          (exprCode, exprEnd) <- compileExprAt baseIndex expr
          let code = exprCode ++ [UnpackToNames names pos]
          pure (code, exprEnd + 1)
        AnnAssignStmt name _ maybeExpr _ ->
          case maybeExpr of
            Nothing -> Right ([], baseIndex)
            Just expr -> do
              (exprCode, exprEnd) <- compileExprAt baseIndex expr
              let code = exprCode ++ [StoreName name]
              pure (code, exprEnd + 1)
        AddAssignStmt name expr pos -> compileCompoundAssign compileExprAt baseIndex name expr pos AddOperator
        SubAssignStmt name expr pos -> compileCompoundAssign compileExprAt baseIndex name expr pos SubtractOperator
        MulAssignStmt name expr pos -> compileCompoundAssign compileExprAt baseIndex name expr pos MultiplyOperator
        DivAssignStmt name expr pos -> compileCompoundAssign compileExprAt baseIndex name expr pos DivideOperator
        ModAssignStmt name expr pos -> compileCompoundAssign compileExprAt baseIndex name expr pos ModuloOperator
        FloorDivAssignStmt name expr pos -> compileCompoundAssign compileExprAt baseIndex name expr pos FloorDivideOperator
        PrintStmt expr _ -> do
          (exprCode, exprEnd) <- compileExprAt baseIndex expr
          let code = exprCode ++ [PrintTop]
          pure (code, exprEnd + 1)
        YieldStmt expr pos ->
          if inFunction
            then compileYieldCollectStmt compileExprAt baseIndex "append" expr pos
            else Left ("VM compile error: unsupported statement at " ++ showPos (stmtPosition stmt))
        YieldFromStmt expr pos ->
          if inFunction
            then compileYieldCollectStmt compileExprAt baseIndex "extend" expr pos
            else Left ("VM compile error: unsupported statement at " ++ showPos (stmtPosition stmt))
        IfStmt cond thenStmts maybeElseStmts _ -> do
          (condCode, condEnd) <- compileExprAt baseIndex cond
          let jumpIfFalseIndex = condEnd
          let thenStartIndex = jumpIfFalseIndex + 1
          (thenCode, thenEndIndex) <- compileStatements thenStartIndex inFunction maybeLoop thenStmts
          case maybeElseStmts of
            Nothing -> do
              let jumpFalseTarget = thenEndIndex
              let code = condCode ++ [JumpIfFalse jumpFalseTarget] ++ thenCode
              pure (code, thenEndIndex)
            Just elseStmts -> do
              let jumpOverElseIndex = thenEndIndex
              let elseStartIndex = jumpOverElseIndex + 1
              (elseCode, elseEndIndex) <- compileStatements elseStartIndex inFunction maybeLoop elseStmts
              let code = condCode ++ [JumpIfFalse elseStartIndex] ++ thenCode ++ [Jump elseEndIndex] ++ elseCode
              pure (code, elseEndIndex)
        WhileStmt cond body whilePos -> do
          (condCode, condEnd) <- compileExprAt baseIndex cond
          let jumpIfFalseIndex = condEnd
          let bodyStartIndex = jumpIfFalseIndex + 1
          let guardIndex = bodyStartIndex
          let firstBodyStmtIndex = guardIndex + 1
          let provisionalLoopContext = Just (0, baseIndex)
          (_, provisionalBodyEndIndex) <- compileStatements firstBodyStmtIndex inFunction provisionalLoopContext body
          let loopEndIndex = provisionalBodyEndIndex + 1
          let loopContext = Just (loopEndIndex, baseIndex)
          (bodyCode, _) <- compileStatements firstBodyStmtIndex inFunction loopContext body
          let code = condCode ++ [JumpIfFalse loopEndIndex, LoopGuard whilePos] ++ bodyCode ++ [Jump baseIndex]
          pure (code, loopEndIndex)
        ForStmt name iterExpr body forPos -> do
          (iterCode, iterEnd) <- compileExprAt baseIndex iterExpr
          let setupIndex = iterEnd
          let nextIndex = setupIndex + 1
          let guardIndex = nextIndex + 1
          let bodyStartIndex = guardIndex + 1
          let provisionalLoopContext = Just (0, nextIndex)
          (_, provisionalBodyEndIndex) <- compileStatements bodyStartIndex inFunction provisionalLoopContext body
          let loopEndIndex = provisionalBodyEndIndex + 1
          let loopContext = Just (loopEndIndex, nextIndex)
          (bodyCode, _) <- compileStatements bodyStartIndex inFunction loopContext body
          let iterPos = exprPosition iterExpr
          let code = iterCode ++ [ForSetup nextIndex iterPos, ForNext name loopEndIndex iterPos, LoopGuard forPos] ++ bodyCode ++ [Jump nextIndex]
          pure (code, loopEndIndex)
        ClassDefStmt className maybeBase body _ ->
          compileClassDefStmt compileDefaults compileStatements compileExprAt baseIndex className maybeBase body Nothing
        FunctionDefStmt name params body posDef ->
          fmap (\(functionCode, _) -> ([DefineFunction name params [] functionCode], baseIndex + 1)) (compileFunctionDefStmt compileStatements compileExprAt posDef [] body)
        FunctionDefDefaultsStmt name params defaults body posDef ->
          fmap (\(functionCode, defaultCodes) -> ([DefineFunction name params defaultCodes functionCode], baseIndex + 1)) (compileFunctionDefStmt compileStatements compileExprAt posDef defaults body)
        ReturnStmt expr _ ->
          if inFunction
            then do
              (exprCode, exprEnd) <- compileExprAt baseIndex expr
              let code = exprCode ++ [ReturnTop]
              pure (code, exprEnd + 1)
            else Left ("VM compile error: unsupported statement at " ++ showPos (stmtPosition stmt))
        BreakStmt pos ->
          case maybeLoop of
            Just (breakTarget, _) -> Right ([Jump breakTarget], baseIndex + 1)
            Nothing -> Left ("Break outside loop at " ++ showPos pos)
        ContinueStmt pos ->
          case maybeLoop of
            Just (_, continueTarget) -> Right ([Jump continueTarget], baseIndex + 1)
            Nothing -> Left ("Continue outside loop at " ++ showPos pos)
