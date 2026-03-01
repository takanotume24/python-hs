module PythonHS.VM.CompileLogicalExpr (compileLogicalExpr) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AndOperator, OrOperator))
import PythonHS.AST.Expr (Expr)
import PythonHS.Evaluator.Value (Value (IntValue))
import PythonHS.VM.Instruction (Instruction (Jump, JumpIfFalse, PushConst), Instruction)

compileLogicalExpr :: (Int -> Expr -> Either String ([Instruction], Int)) -> BinaryOperator -> Int -> Expr -> Expr -> Either String ([Instruction], Int)
compileLogicalExpr compileExprAt op baseIndex left right =
  case op of
    AndOperator -> do
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
    OrOperator -> do
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
    _ -> error "compileLogicalExpr only supports AndOperator/OrOperator"
