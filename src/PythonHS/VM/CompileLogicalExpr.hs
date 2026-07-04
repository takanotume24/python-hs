module PythonHS.VM.CompileLogicalExpr (compileLogicalExpr) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AndOperator, OrOperator))
import PythonHS.AST.Expr (Expr)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.CompileLogicalExprConfig (CompileLogicalExprConfig (..))
import PythonHS.VM.Instruction (Instruction (..))

compileLogicalExpr :: CompileLogicalExprConfig -> Either String CompileExprResult
compileLogicalExpr config =
  let compileExpr = compileLogicalExprCompileExpr config
      op = compileLogicalExprOp config
      baseIndex = compileLogicalExprBaseIndex config
      left = compileLogicalExprLeft config
      right = compileLogicalExprRight config
   in case op of
        AndOperator -> do
          leftResult <- compileExpr baseIndex left
          let firstJumpIndex = compileExprResultEndIndex leftResult
          let rightStartIndex = firstJumpIndex + 1
          rightResult <- compileExpr rightStartIndex right
          let secondJumpIndex = compileExprResultEndIndex rightResult
          let truePushIndex = secondJumpIndex + 1
          let jumpEndIndex = truePushIndex + 1
          let falsePushIndex = jumpEndIndex + 1
          let endIndex = falsePushIndex + 1
          let code =
                compileExprResultCode leftResult
                  ++ [JumpIfFalse {jumpIfFalseTarget = falsePushIndex}]
                  ++ compileExprResultCode rightResult
                  ++ [JumpIfFalse {jumpIfFalseTarget = falsePushIndex}, PushConst {pushConstValue = IntValue {intValue = 1}}, Jump {jumpTarget = endIndex}, PushConst {pushConstValue = IntValue {intValue = 0}}]
          pure (CompileExprResult {compileExprResultCode = code, compileExprResultEndIndex = endIndex})
        OrOperator -> do
          leftResult <- compileExpr baseIndex left
          let jumpEvalRightIndex = compileExprResultEndIndex leftResult
          let trueFromLeftIndex = jumpEvalRightIndex + 1
          let jumpEndFromLeftIndex = trueFromLeftIndex + 1
          let rightStartIndex = jumpEndFromLeftIndex + 1
          rightResult <- compileExpr rightStartIndex right
          let jumpFalseIndex = compileExprResultEndIndex rightResult
          let trueFromRightIndex = jumpFalseIndex + 1
          let jumpEndFromRightIndex = trueFromRightIndex + 1
          let falsePushIndex = jumpEndFromRightIndex + 1
          let endIndex = falsePushIndex + 1
          let code =
                compileExprResultCode leftResult
                  ++ [JumpIfFalse {jumpIfFalseTarget = rightStartIndex}, PushConst {pushConstValue = IntValue {intValue = 1}}, Jump {jumpTarget = endIndex}]
                  ++ compileExprResultCode rightResult
                  ++ [JumpIfFalse {jumpIfFalseTarget = falsePushIndex}, PushConst {pushConstValue = IntValue {intValue = 1}}, Jump {jumpTarget = endIndex}, PushConst {pushConstValue = IntValue {intValue = 0}}]
          pure (CompileExprResult {compileExprResultCode = code, compileExprResultEndIndex = endIndex})
        _ -> error "compileLogicalExpr only supports AndOperator/OrOperator"
