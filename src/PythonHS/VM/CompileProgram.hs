module PythonHS.VM.CompileProgram (compileProgram) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AddOperator, AndOperator, DivideOperator, FloorDivideOperator, ModuloOperator, MultiplyOperator, OrOperator, SubtractOperator))
import PythonHS.AST.Expr (Expr (BinaryExpr, CallExpr, DictExpr, FloatExpr, IdentifierExpr, IntegerExpr, ListExpr, NoneExpr, NotExpr, StringExpr, UnaryMinusExpr))
import PythonHS.AST.Program (Program (Program))
import PythonHS.AST.Stmt (Stmt (AddAssignStmt, AssignStmt, BreakStmt, ContinueStmt, DivAssignStmt, FloorDivAssignStmt, ForStmt, FromImportStmt, FunctionDefDefaultsStmt, FunctionDefStmt, GlobalStmt, IfStmt, ImportStmt, ModAssignStmt, MulAssignStmt, PassStmt, PrintStmt, RaiseStmt, ReturnStmt, SubAssignStmt, TryExceptStmt, WhileStmt))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (FloatValue, IntValue, NoneValue, StringValue))
import PythonHS.VM.CompileCallArgsAt (compileCallArgsAt)
import PythonHS.VM.CompileDefaults (compileDefaults)
import PythonHS.VM.CompileExprItemsAt (compileExprItemsAt)
import PythonHS.VM.CompileImportStmt (compileImportStmt)
import PythonHS.VM.CompileLogicalExpr (compileLogicalExpr)
import PythonHS.VM.ExprPosition (exprPosition)
import PythonHS.VM.Instruction (Instruction (ApplyBinary, ApplyNot, ApplyUnaryMinus, BuildDict, BuildList, CallFunction, DeclareGlobal, DefineFunction, ForNext, ForSetup, Halt, Jump, JumpIfFalse, LoadName, LoopGuard, PopExceptionHandler, PrintTop, PushConst, PushExceptionHandler, RaiseTop, ReturnTop, StoreName))
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
        GlobalStmt name _ -> Right ([DeclareGlobal name], baseIndex + 1)
        RaiseStmt expr pos -> do
          (exprCode, exprEnd) <- compileExprAt baseIndex expr
          let code = exprCode ++ [RaiseTop pos]
          pure (code, exprEnd + 1)
        TryExceptStmt tryStmts exceptStmts _ -> do
          let tryStartIndex = baseIndex + 1
          (tryCode, tryEndIndex) <- compileStatements tryStartIndex inFunction maybeLoop tryStmts
          let popHandlerIndex = tryEndIndex
          let jumpEndIndex = popHandlerIndex + 1
          let exceptStartIndex = jumpEndIndex + 1
          (exceptCode, exceptEndIndex) <- compileStatements exceptStartIndex inFunction maybeLoop exceptStmts
          let code =
                [PushExceptionHandler exceptStartIndex]
                  ++ tryCode
                  ++ [PopExceptionHandler, Jump exceptEndIndex]
                  ++ exceptCode
          pure (code, exceptEndIndex)
        ImportStmt _ _ ->
          compileImportStmt baseIndex stmt
        FromImportStmt _ _ _ ->
          compileImportStmt baseIndex stmt
        AssignStmt name expr _ -> do
          (exprCode, exprEnd) <- compileExprAt baseIndex expr
          let code = exprCode ++ [StoreName name]
          pure (code, exprEnd + 1)
        AddAssignStmt name expr pos -> compileCompoundAssign baseIndex name expr pos AddOperator
        SubAssignStmt name expr pos -> compileCompoundAssign baseIndex name expr pos SubtractOperator
        MulAssignStmt name expr pos -> compileCompoundAssign baseIndex name expr pos MultiplyOperator
        DivAssignStmt name expr pos -> compileCompoundAssign baseIndex name expr pos DivideOperator
        ModAssignStmt name expr pos -> compileCompoundAssign baseIndex name expr pos ModuloOperator
        FloorDivAssignStmt name expr pos -> compileCompoundAssign baseIndex name expr pos FloorDivideOperator
        PrintStmt expr _ -> do
          (exprCode, exprEnd) <- compileExprAt baseIndex expr
          let code = exprCode ++ [PrintTop]
          pure (code, exprEnd + 1)
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
        FunctionDefStmt name params body _ -> do
          (bodyCode, _) <- compileStatements 0 True Nothing body
          let functionCode = bodyCode ++ [PushConst (IntValue 0), ReturnTop]
          pure ([DefineFunction name params [] functionCode], baseIndex + 1)
        FunctionDefDefaultsStmt name params defaults body _ -> do
          (defaultCodes, _) <- compileDefaults compileExprAt defaults
          (bodyCode, _) <- compileStatements 0 True Nothing body
          let functionCode = bodyCode ++ [PushConst (IntValue 0), ReturnTop]
          pure ([DefineFunction name params defaultCodes functionCode], baseIndex + 1)
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

    compileCompoundAssign baseIndex name expr pos op = do
      (exprCode, exprEnd) <- compileExprAt (baseIndex + 1) expr
      let code = [LoadName name pos] ++ exprCode ++ [ApplyBinary op pos, StoreName name]
      pure (code, exprEnd + 2)

    compileExprAt baseIndex expr =
      case expr of
        IntegerExpr n _ -> Right ([PushConst (IntValue n)], baseIndex + 1)
        FloatExpr n _ -> Right ([PushConst (FloatValue n)], baseIndex + 1)
        StringExpr s _ -> Right ([PushConst (StringValue s)], baseIndex + 1)
        NoneExpr _ -> Right ([PushConst NoneValue], baseIndex + 1)
        ListExpr elements _ -> do
          (elementCode, elementEnd) <- compileExprItemsAt compileExprAt baseIndex elements
          pure (elementCode ++ [BuildList (length elements)], elementEnd + 1)
        DictExpr entries _ -> do
          (entryCode, entryEnd) <- compileDictEntriesAt baseIndex entries
          pure (entryCode ++ [BuildDict (length entries)], entryEnd + 1)
        IdentifierExpr name pos -> Right ([LoadName name pos], baseIndex + 1)
        UnaryMinusExpr unaryExpr pos -> do
          (exprCode, exprEnd) <- compileExprAt baseIndex unaryExpr
          pure (exprCode ++ [ApplyUnaryMinus pos], exprEnd + 1)
        NotExpr notExpr pos -> do
          (exprCode, exprEnd) <- compileExprAt baseIndex notExpr
          pure (exprCode ++ [ApplyNot pos], exprEnd + 1)
        BinaryExpr AndOperator left right _ -> compileLogicalExpr compileExprAt AndOperator baseIndex left right
        BinaryExpr OrOperator left right _ -> compileLogicalExpr compileExprAt OrOperator baseIndex left right
        BinaryExpr op left right pos -> do
          (leftCode, leftEnd) <- compileExprAt baseIndex left
          (rightCode, rightEnd) <- compileExprAt leftEnd right
          pure (leftCode ++ rightCode ++ [ApplyBinary op pos], rightEnd + 1)
        CallExpr fname args pos -> do
          compiledArgs <- compileCallArgsAt compileExprAt args
          pure ([CallFunction fname compiledArgs pos], baseIndex + 1)
        _ -> Left ("VM compile error: unsupported expression at " ++ showPos (exprPosition expr))

    compileDictEntriesAt baseIndex entries =
      case entries of
        [] -> Right ([], baseIndex)
        (keyExpr, valueExpr) : rest -> do
          (keyCode, keyEnd) <- compileExprAt baseIndex keyExpr
          (valueCode, valueEnd) <- compileExprAt keyEnd valueExpr
          (restCode, restEnd) <- compileDictEntriesAt valueEnd rest
          pure (keyCode ++ valueCode ++ restCode, restEnd)
