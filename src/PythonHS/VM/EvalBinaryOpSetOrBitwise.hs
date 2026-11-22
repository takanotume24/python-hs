module PythonHS.VM.EvalBinaryOpSetOrBitwise (evalBinaryOpSetOrBitwise) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.VM.EvalBinaryOpBitwise (evalBinaryOpBitwise)
import PythonHS.VM.EvalBinaryOpBitwiseConfig (EvalBinaryOpBitwiseConfig (..))
import PythonHS.VM.EvalBinaryOpSet (evalBinaryOpSet)
import PythonHS.VM.EvalBinaryOpSetConfig (EvalBinaryOpSetConfig (..))
import PythonHS.VM.EvalBinaryOpSetOrBitwiseConfig (EvalBinaryOpSetOrBitwiseConfig (..))

-- | Prefer set semantics for @&@, @|@ and @^@ when both operands are sets,
-- then fall back to the integer bitwise implementation.
evalBinaryOpSetOrBitwise :: EvalBinaryOpSetOrBitwiseConfig -> Either String Value
evalBinaryOpSetOrBitwise config =
  case evalBinaryOpSet setConfig of
    Just result -> result
    Nothing -> evalBinaryOpBitwise bitwiseConfig
  where
    left = evalBinaryOpSetOrBitwiseLeft config
    right = evalBinaryOpSetOrBitwiseRight config
    op = evalBinaryOpSetOrBitwiseOp config
    pos = evalBinaryOpSetOrBitwisePos config
    setConfig = EvalBinaryOpSetConfig {evalBinaryOpSetLeft = left, evalBinaryOpSetRight = right, evalBinaryOpSetOp = op, evalBinaryOpSetPos = pos}
    bitwiseConfig = EvalBinaryOpBitwiseConfig {evalBinaryOpBitwiseConfigLeft = left, evalBinaryOpBitwiseConfigRight = right, evalBinaryOpBitwiseConfigOp = op, evalBinaryOpBitwiseConfigPos = pos}
