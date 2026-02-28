module PythonHS.VM.CompileProgram (compileProgram) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AddOperator))
import PythonHS.AST.Expr (Expr (BinaryExpr, IdentifierExpr, IntegerExpr, StringExpr))
import PythonHS.AST.Program (Program (Program))
import PythonHS.AST.Stmt (Stmt (AssignStmt, IfStmt, PrintStmt, WhileStmt))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (IntValue, StringValue))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.VM.Instruction (Instruction (AddValues, Halt, Jump, JumpIfFalse, LoadName, PrintTop, PushConst, StoreName))

compileProgram :: Program -> Either String [Instruction]
compileProgram (Program stmts) = do
  (compiled, nextIndex) <- compileStatements 0 stmts
  if nextIndex == length compiled
    then pure (compiled ++ [Halt])
    else Left "VM compile error: internal instruction index mismatch"
  where
    compileStatements baseIndex items =
      case items of
        [] -> Right ([], baseIndex)
        stmt : rest -> do
          (stmtCode, afterStmt) <- compileStmt baseIndex stmt
          (restCode, afterRest) <- compileStatements afterStmt rest
          pure (stmtCode ++ restCode, afterRest)

    compileStmt baseIndex stmt =
      case stmt of
        AssignStmt name expr _ -> do
          exprCode <- compileExpr expr
          let code = exprCode ++ [StoreName name]
          pure (code, baseIndex + length code)
        PrintStmt expr _ -> do
          exprCode <- compileExpr expr
          let code = exprCode ++ [PrintTop]
          pure (code, baseIndex + length code)
        IfStmt cond thenStmts maybeElseStmts _ -> do
          condCode <- compileExpr cond
          let jumpIfFalseIndex = baseIndex + length condCode
          let thenStartIndex = jumpIfFalseIndex + 1
          (thenCode, thenEndIndex) <- compileStatements thenStartIndex thenStmts
          case maybeElseStmts of
            Nothing -> do
              let jumpFalseTarget = thenEndIndex
              let code = condCode ++ [JumpIfFalse jumpFalseTarget] ++ thenCode
              pure (code, thenEndIndex)
            Just elseStmts -> do
              let jumpOverElseIndex = thenEndIndex
              let elseStartIndex = jumpOverElseIndex + 1
              (elseCode, elseEndIndex) <- compileStatements elseStartIndex elseStmts
              let code = condCode ++ [JumpIfFalse elseStartIndex] ++ thenCode ++ [Jump elseEndIndex] ++ elseCode
              pure (code, elseEndIndex)
        WhileStmt cond body _ -> do
          condCode <- compileExpr cond
          let jumpIfFalseIndex = baseIndex + length condCode
          let bodyStartIndex = jumpIfFalseIndex + 1
          (bodyCode, bodyEndIndex) <- compileStatements bodyStartIndex body
          let loopEndIndex = bodyEndIndex + 1
          let code = condCode ++ [JumpIfFalse loopEndIndex] ++ bodyCode ++ [Jump baseIndex]
          pure (code, loopEndIndex)
        _ -> Left ("VM compile error: unsupported statement at " ++ showPos (stmtPosition stmt))

    compileExpr expr =
      case expr of
        IntegerExpr n _ -> Right [PushConst (IntValue n)]
        StringExpr s _ -> Right [PushConst (StringValue s)]
        IdentifierExpr name pos -> Right [LoadName name pos]
        BinaryExpr AddOperator left right pos -> do
          leftCode <- compileExpr left
          rightCode <- compileExpr right
          pure (leftCode ++ rightCode ++ [AddValues pos])
        _ -> Left ("VM compile error: unsupported expression at " ++ showPos (exprPosition expr))

    stmtPosition stmt =
      case stmt of
        AssignStmt _ _ pos -> pos
        IfStmt _ _ _ pos -> pos
        WhileStmt _ _ pos -> pos
        PrintStmt _ pos -> pos
        _ -> Position 1 1

    exprPosition expr =
      case expr of
        IntegerExpr _ pos -> pos
        StringExpr _ pos -> pos
        IdentifierExpr _ pos -> pos
        BinaryExpr _ _ _ pos -> pos
        _ -> Position 1 1
