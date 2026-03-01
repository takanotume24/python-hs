module PythonHS.VM.CompileProgram (compileProgram) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AndOperator, OrOperator))
import PythonHS.AST.Expr (Expr (BinaryExpr, CallExpr, DictExpr, FloatExpr, IdentifierExpr, IntegerExpr, ListExpr, NoneExpr, NotExpr, StringExpr, UnaryMinusExpr))
import PythonHS.AST.Program (Program (Program))
import PythonHS.AST.Stmt (Stmt (AssignStmt, BreakStmt, ContinueStmt, ForStmt, FunctionDefDefaultsStmt, FunctionDefStmt, IfStmt, PassStmt, PrintStmt, ReturnStmt, WhileStmt))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (FloatValue, IntValue, NoneValue, StringValue))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.VM.CompileExprItemsAt (compileExprItemsAt)
import PythonHS.VM.ExprPosition (exprPosition)
import PythonHS.VM.Instruction (Instruction (ApplyBinary, ApplyNot, ApplyUnaryMinus, BuildDict, BuildList, CallFunction, DefineFunction, ForNext, ForSetup, Halt, Jump, JumpIfFalse, LoadName, LoopGuard, PrintTop, PushConst, ReturnTop, StoreName))

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
        AssignStmt name expr _ -> do
          (exprCode, exprEnd) <- compileExprAt baseIndex expr
          let code = exprCode ++ [StoreName name]
          pure (code, exprEnd + 1)
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
          let functionCode = bodyCode ++ [PushConst NoneValue, ReturnTop]
          pure ([DefineFunction name params functionCode], baseIndex + 1)
        FunctionDefDefaultsStmt _ _ _ _ pos -> Left ("VM compile error: unsupported statement at " ++ showPos pos)
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
        _ -> Left ("VM compile error: unsupported statement at " ++ showPos (stmtPosition stmt))

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
        BinaryExpr AndOperator left right _ -> compileAndExpr baseIndex left right
        BinaryExpr OrOperator left right _ -> compileOrExpr baseIndex left right
        BinaryExpr op left right pos -> do
          (leftCode, leftEnd) <- compileExprAt baseIndex left
          (rightCode, rightEnd) <- compileExprAt leftEnd right
          pure (leftCode ++ rightCode ++ [ApplyBinary op pos], rightEnd + 1)
        CallExpr fname args pos -> do
          (argsCode, argsEnd) <- compileExprItemsAt compileExprAt baseIndex args
          pure (argsCode ++ [CallFunction fname (length args) pos], argsEnd + 1)
        _ -> Left ("VM compile error: unsupported expression at " ++ showPos (exprPosition expr))

    compileDictEntriesAt baseIndex entries =
      case entries of
        [] -> Right ([], baseIndex)
        (keyExpr, valueExpr) : rest -> do
          (keyCode, keyEnd) <- compileExprAt baseIndex keyExpr
          (valueCode, valueEnd) <- compileExprAt keyEnd valueExpr
          (restCode, restEnd) <- compileDictEntriesAt valueEnd rest
          pure (keyCode ++ valueCode ++ restCode, restEnd)

    compileAndExpr baseIndex left right = do
      (leftCode, leftEnd) <- compileExprAt baseIndex left
      let firstJumpIndex = leftEnd
      let rightStartIndex = firstJumpIndex + 1
      (rightCode, rightEnd) <- compileExprAt rightStartIndex right
      let secondJumpIndex = rightEnd
      let truePushIndex = secondJumpIndex + 1
      let jumpEndIndex = truePushIndex + 1
      let falsePushIndex = jumpEndIndex + 1
      let endIndex = falsePushIndex + 1
      let code =
            leftCode
              ++ [JumpIfFalse falsePushIndex]
              ++ rightCode
              ++ [JumpIfFalse falsePushIndex, PushConst (IntValue 1), Jump endIndex, PushConst (IntValue 0)]
      pure (code, endIndex)

    compileOrExpr baseIndex left right = do
      (leftCode, leftEnd) <- compileExprAt baseIndex left
      let jumpEvalRightIndex = leftEnd
      let trueFromLeftIndex = jumpEvalRightIndex + 1
      let jumpEndFromLeftIndex = trueFromLeftIndex + 1
      let rightStartIndex = jumpEndFromLeftIndex + 1
      (rightCode, rightEnd) <- compileExprAt rightStartIndex right
      let jumpFalseIndex = rightEnd
      let trueFromRightIndex = jumpFalseIndex + 1
      let jumpEndFromRightIndex = trueFromRightIndex + 1
      let falsePushIndex = jumpEndFromRightIndex + 1
      let endIndex = falsePushIndex + 1
      let code =
            leftCode
              ++ [JumpIfFalse rightStartIndex, PushConst (IntValue 1), Jump endIndex]
              ++ rightCode
              ++ [JumpIfFalse falsePushIndex, PushConst (IntValue 1), Jump endIndex, PushConst (IntValue 0)]
      pure (code, endIndex)

    stmtPosition stmt =
      case stmt of
        AssignStmt _ _ pos -> pos
        FunctionDefStmt _ _ _ pos -> pos
        FunctionDefDefaultsStmt _ _ _ _ pos -> pos
        BreakStmt pos -> pos
        ContinueStmt pos -> pos
        ForStmt _ _ _ pos -> pos
        IfStmt _ _ _ pos -> pos
        PassStmt pos -> pos
        ReturnStmt _ pos -> pos
        WhileStmt _ _ pos -> pos
        PrintStmt _ pos -> pos
        _ -> Position 1 1
