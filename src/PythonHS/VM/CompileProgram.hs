module PythonHS.VM.CompileProgram (compileProgram) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AddOperator, AndOperator, DivideOperator, FloorDivideOperator, ModuloOperator, MultiplyOperator, OrOperator, SubtractOperator))
import PythonHS.AST.Expr (Expr (BinaryExpr, CallExpr, CallValueExpr, DictExpr, FloatExpr, IdentifierExpr, IntegerExpr, LambdaDefaultsExpr, LambdaExpr, ListComprehensionClausesExpr, ListComprehensionExpr, ListExpr, NoneExpr, NotExpr, StringExpr, UnaryMinusExpr, WalrusExpr))
import PythonHS.AST.Program (Program (Program))
import PythonHS.AST.Stmt (Stmt (AddAssignStmt, AssignStmt, BreakStmt, ClassDefStmt, ContinueStmt, DecoratedStmt, DivAssignStmt, FloorDivAssignStmt, ForStmt, FromImportStmt, FunctionDefDefaultsStmt, FunctionDefStmt, GlobalStmt, IfStmt, ImportStmt, MatchStmt, ModAssignStmt, MulAssignStmt, PassStmt, PrintStmt, RaiseStmt, ReturnStmt, SubAssignStmt, TryExceptStmt, WhileStmt, YieldFromStmt, YieldStmt))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (FloatValue, IntValue, NoneValue, StringValue))
import PythonHS.VM.CompileCallArgsAt (compileCallArgsAt)
import PythonHS.VM.CompileClassStmt (compileClassStmt)
import PythonHS.VM.CompileCompoundAssign (compileCompoundAssign)
import PythonHS.VM.CompileDictEntriesAt (compileDictEntriesAt)
import PythonHS.VM.CompileDefaults (compileDefaults)
import PythonHS.VM.CompileDecoratedStmt (compileDecoratedStmt)
import PythonHS.VM.CompileExprItemsAt (compileExprItemsAt)
import PythonHS.VM.CompileFunctionDefStmt (compileFunctionDefStmt)
import PythonHS.VM.CompileImportStmt (compileImportStmt)
import PythonHS.VM.CompileLogicalExpr (compileLogicalExpr)
import PythonHS.VM.CompileMatch (compileMatch)
import PythonHS.VM.CompileTryExcept (compileTryExcept)
import PythonHS.VM.ExprPosition (exprPosition)
import PythonHS.VM.CompileComprehensionClauses (compileComprehensionClauses)
import PythonHS.VM.CompileYieldCollectStmt (compileYieldCollectStmt)
import PythonHS.VM.Instruction (Instruction (ApplyBinary, ApplyNot, ApplyUnaryMinus, BuildDict, BuildList, BuildListComprehension, CallFunction, CallValueFunction, CreateLambda, DeclareGlobal, DefineFunction, DupTop, ForNext, ForSetup, Halt, Jump, JumpIfFalse, LoadName, LoopGuard, PrintTop, PushConst, RaiseTop, ReturnTop, StoreName))
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
          compileDecoratedStmt compileStmt compileExprAt baseIndex inFunction maybeLoop decorators targetStmt
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
          compileClassStmt compileDefaults compileStatements compileExprAt baseIndex className maybeBase body
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

    compileExprAt baseIndex expr =
      case expr of
        IntegerExpr n _ -> Right ([PushConst (IntValue n)], baseIndex + 1)
        FloatExpr n _ -> Right ([PushConst (FloatValue n)], baseIndex + 1)
        StringExpr s _ -> Right ([PushConst (StringValue s)], baseIndex + 1)
        NoneExpr _ -> Right ([PushConst NoneValue], baseIndex + 1)
        ListExpr elements _ -> do
          (elementCode, elementEnd) <- compileExprItemsAt compileExprAt baseIndex elements
          pure (elementCode ++ [BuildList (length elements)], elementEnd + 1)
        ListComprehensionExpr valueExpr loopName iterExpr pos -> do
          (iterCode, _) <- compileExprAt 0 iterExpr
          (valueCode, _) <- compileExprAt 0 valueExpr
          let clauses = [([loopName], iterCode ++ [ReturnTop], [])]
          pure ([BuildListComprehension clauses (valueCode ++ [ReturnTop]) pos], baseIndex + 1)
        ListComprehensionClausesExpr valueExpr clausesExpr pos -> do
          clauses <- compileComprehensionClauses compileExprAt clausesExpr
          (valueCode, _) <- compileExprAt 0 valueExpr
          pure ([BuildListComprehension clauses (valueCode ++ [ReturnTop]) pos], baseIndex + 1)
        DictExpr entries _ -> do
          (entryCode, entryEnd) <- compileDictEntriesAt compileExprAt baseIndex entries
          pure (entryCode ++ [BuildDict (length entries)], entryEnd + 1)
        IdentifierExpr name pos -> Right ([LoadName name pos], baseIndex + 1)
        UnaryMinusExpr unaryExpr pos -> do
          (exprCode, exprEnd) <- compileExprAt baseIndex unaryExpr
          pure (exprCode ++ [ApplyUnaryMinus pos], exprEnd + 1)
        NotExpr notExpr pos -> do
          (exprCode, exprEnd) <- compileExprAt baseIndex notExpr
          pure (exprCode ++ [ApplyNot pos], exprEnd + 1)
        WalrusExpr name valueExpr _ -> do
          (valueCode, valueEnd) <- compileExprAt baseIndex valueExpr
          pure (valueCode ++ [DupTop, StoreName name], valueEnd + 2)
        LambdaExpr params bodyExpr pos -> do
          (bodyCode, _) <- compileExprAt 0 bodyExpr
          let lambdaName = "__lambda_" ++ showPos pos
          pure ([CreateLambda lambdaName params [] (bodyCode ++ [ReturnTop])], baseIndex + 1)
        LambdaDefaultsExpr params defaults bodyExpr pos -> do
          (defaultCodes, _) <- compileDefaults compileExprAt defaults
          (bodyCode, _) <- compileExprAt 0 bodyExpr
          let lambdaName = "__lambda_" ++ showPos pos
          pure ([CreateLambda lambdaName params defaultCodes (bodyCode ++ [ReturnTop])], baseIndex + 1)
        BinaryExpr AndOperator left right _ -> compileLogicalExpr compileExprAt AndOperator baseIndex left right
        BinaryExpr OrOperator left right _ -> compileLogicalExpr compileExprAt OrOperator baseIndex left right
        BinaryExpr op left right pos -> do
          (leftCode, leftEnd) <- compileExprAt baseIndex left
          (rightCode, rightEnd) <- compileExprAt leftEnd right
          pure (leftCode ++ rightCode ++ [ApplyBinary op pos], rightEnd + 1)
        CallExpr fname args pos -> do
          compiledArgs <- compileCallArgsAt compileExprAt args
          pure ([CallFunction fname compiledArgs pos], baseIndex + 1)
        CallValueExpr callee args pos -> do
          (calleeCode, calleeEnd) <- compileExprAt baseIndex callee
          compiledArgs <- compileCallArgsAt compileExprAt args
          pure (calleeCode ++ [CallValueFunction compiledArgs pos], calleeEnd + 1)
        _ -> Left ("VM compile error: unsupported expression at " ++ showPos (exprPosition expr))
