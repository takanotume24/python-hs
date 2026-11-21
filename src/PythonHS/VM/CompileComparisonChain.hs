module PythonHS.VM.CompileComparisonChain (compileComparisonChain) where

import PythonHS.AST.BinaryOperator (BinaryOperator)
import PythonHS.AST.Expr (Expr)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.VM.CompileComparisonChainBodyResult (CompileComparisonChainBodyResult (..))
import PythonHS.VM.CompileComparisonChainConfig (CompileComparisonChainConfig (..))
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.Instruction (Instruction (..))

compileComparisonChain :: CompileComparisonChainConfig -> Either String CompileExprResult
compileComparisonChain config = do
  let compileExprFn = compileComparisonChainCompileExpr config
      baseIndex = compileComparisonChainBaseIndex config
      leftExpr = compileComparisonChainLeft config
      ops = compileComparisonChainOps config
      rights = compileComparisonChainRights config
      pos = compileComparisonChainPos config
      prefix = chainNamePrefix pos
  leftResult <- compileExprFn baseIndex leftExpr
  let leftCode = compileExprResultCode leftResult
      leftEnd = compileExprResultEndIndex leftResult
      storeLeft = [StoreName {storeNameName = prefix ++ "_0"}]
      loadLeft = [LoadName {loadNameName = prefix ++ "_0", loadNamePos = pos}]
  bodyResult <- compileItems compileExprFn prefix pos (leftEnd + 2) ops rights 0
  let bodyCode = compileComparisonChainBodyCode bodyResult
      bodyEnd = compileComparisonChainBodyEndIndex bodyResult
      failLabel = bodyEnd + 3
      endLabel = bodyEnd + 4
      patchedBodyCode = map (patchJumpIfFalse failLabel) bodyCode
      finalCode =
        leftCode
          ++ storeLeft
          ++ loadLeft
          ++ patchedBodyCode
          ++ [ PopTop,
               PushConst {pushConstValue = IntValue {intValue = 1}},
               Jump {jumpTarget = endLabel},
               PushConst {pushConstValue = IntValue {intValue = 0}}
             ]
      finalEnd = endLabel
  Right (CompileExprResult {compileExprResultCode = finalCode, compileExprResultEndIndex = finalEnd})
  where
    compileItems _ _ _ endIndex [] [] _ = Right (CompileComparisonChainBodyResult {compileComparisonChainBodyCode = [], compileComparisonChainBodyEndIndex = endIndex})
    compileItems compileExprFn' prefix pos endIndex (op : restOps) (rightExpr : restRights) idx = do
      rightResult <- compileExprFn' endIndex rightExpr
      let rightCode = compileExprResultCode rightResult
          rightEnd = compileExprResultEndIndex rightResult
          nextIdx = idx + 1
          storeName = prefix ++ "_" ++ show nextIdx
          comparisonCode =
            [ StoreName {storeNameName = storeName},
              LoadName {loadNameName = prefix ++ "_" ++ show idx, loadNamePos = pos},
              LoadName {loadNameName = storeName, loadNamePos = pos},
              ApplyBinary {applyBinaryOp = op, applyBinaryPos = pos},
              JumpIfFalse {jumpIfFalseTarget = 0},
              PopTop,
              LoadName {loadNameName = storeName, loadNamePos = pos}
            ]
          currentEnd = rightEnd + length comparisonCode
      restResult <- compileItems compileExprFn' prefix pos currentEnd restOps restRights nextIdx
      Right (CompileComparisonChainBodyResult {compileComparisonChainBodyCode = rightCode ++ comparisonCode ++ compileComparisonChainBodyCode restResult, compileComparisonChainBodyEndIndex = compileComparisonChainBodyEndIndex restResult})

    chainNamePrefix pos' = "__cmpchain_l" ++ show (line pos') ++ "_c" ++ show (column pos')

    patchJumpIfFalse target instruction =
      case instruction of
        JumpIfFalse {} -> instruction {jumpIfFalseTarget = target}
        _ -> instruction
