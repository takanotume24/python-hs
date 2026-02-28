module PythonHS.VM.CompileProgram (compileProgram) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AndOperator, OrOperator))
import PythonHS.AST.Expr (Expr (BinaryExpr, CallExpr, FloatExpr, IdentifierExpr, IntegerExpr, NoneExpr, NotExpr, StringExpr, UnaryMinusExpr))
import PythonHS.AST.Program (Program (Program))
import PythonHS.AST.Stmt (Stmt (AssignStmt, FunctionDefDefaultsStmt, FunctionDefStmt, IfStmt, PrintStmt, ReturnStmt, WhileStmt))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (FloatValue, IntValue, NoneValue, StringValue))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.VM.Instruction (Instruction (ApplyBinary, ApplyNot, ApplyUnaryMinus, CallFunction, DefineFunction, Halt, Jump, JumpIfFalse, LoadName, PrintTop, PushConst, ReturnTop, StoreName))

compileProgram :: Program -> Either String [Instruction]
compileProgram (Program stmts) = do
  (compiled, nextIndex) <- compileStatements 0 False stmts
  if nextIndex == length compiled
    then pure (compiled ++ [Halt])
    else Left "VM compile error: internal instruction index mismatch"
  where
    compileStatements baseIndex inFunction items =
      case items of
        [] -> Right ([], baseIndex)
        stmt : rest -> do
          (stmtCode, afterStmt) <- compileStmt baseIndex inFunction stmt
          (restCode, afterRest) <- compileStatements afterStmt inFunction rest
          pure (stmtCode ++ restCode, afterRest)

    compileStmt baseIndex inFunction stmt =
      case stmt of
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
          (thenCode, thenEndIndex) <- compileStatements thenStartIndex inFunction thenStmts
          case maybeElseStmts of
            Nothing -> do
              let jumpFalseTarget = thenEndIndex
              let code = condCode ++ [JumpIfFalse jumpFalseTarget] ++ thenCode
              pure (code, thenEndIndex)
            Just elseStmts -> do
              let jumpOverElseIndex = thenEndIndex
              let elseStartIndex = jumpOverElseIndex + 1
              (elseCode, elseEndIndex) <- compileStatements elseStartIndex inFunction elseStmts
              let code = condCode ++ [JumpIfFalse elseStartIndex] ++ thenCode ++ [Jump elseEndIndex] ++ elseCode
              pure (code, elseEndIndex)
        WhileStmt cond body _ -> do
          (condCode, condEnd) <- compileExprAt baseIndex cond
          let jumpIfFalseIndex = condEnd
          let bodyStartIndex = jumpIfFalseIndex + 1
          (bodyCode, bodyEndIndex) <- compileStatements bodyStartIndex inFunction body
          let loopEndIndex = bodyEndIndex + 1
          let code = condCode ++ [JumpIfFalse loopEndIndex] ++ bodyCode ++ [Jump baseIndex]
          pure (code, loopEndIndex)
        FunctionDefStmt name params body _ -> do
          (bodyCode, _) <- compileStatements 0 True body
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
        _ -> Left ("VM compile error: unsupported statement at " ++ showPos (stmtPosition stmt))

    compileExprAt baseIndex expr =
      case expr of
        IntegerExpr n _ -> Right ([PushConst (IntValue n)], baseIndex + 1)
        FloatExpr n _ -> Right ([PushConst (FloatValue n)], baseIndex + 1)
        StringExpr s _ -> Right ([PushConst (StringValue s)], baseIndex + 1)
        NoneExpr _ -> Right ([PushConst NoneValue], baseIndex + 1)
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
          (argsCode, argsEnd) <- compileCallArgs baseIndex args
          pure (argsCode ++ [CallFunction fname (length args) pos], argsEnd + 1)
        _ -> Left ("VM compile error: unsupported expression at " ++ showPos (exprPosition expr))

    compileCallArgs baseIndex args =
      case args of
        [] -> Right ([], baseIndex)
        arg : rest -> do
          (argCode, argEnd) <- compileExprAt baseIndex arg
          (restCode, restEnd) <- compileCallArgs argEnd rest
          pure (argCode ++ restCode, restEnd)

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
        IfStmt _ _ _ pos -> pos
        ReturnStmt _ pos -> pos
        WhileStmt _ _ pos -> pos
        PrintStmt _ pos -> pos
        _ -> Position 1 1

    exprPosition expr =
      case expr of
        IntegerExpr _ pos -> pos
        FloatExpr _ pos -> pos
        StringExpr _ pos -> pos
        NoneExpr pos -> pos
        IdentifierExpr _ pos -> pos
        UnaryMinusExpr _ pos -> pos
        NotExpr _ pos -> pos
        BinaryExpr _ _ _ pos -> pos
        CallExpr _ _ pos -> pos
        _ -> Position 1 1
