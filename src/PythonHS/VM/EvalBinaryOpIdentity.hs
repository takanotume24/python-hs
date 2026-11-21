module PythonHS.VM.EvalBinaryOpIdentity (evalBinaryOpIdentity) where

import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.EvalBinaryOpIdentityConfig (EvalBinaryOpIdentityConfig (..))

evalBinaryOpIdentity :: EvalBinaryOpIdentityConfig -> Either String Value
evalBinaryOpIdentity config =
  let left = evalBinaryOpIdentityConfigLeft config
      right = evalBinaryOpIdentityConfigRight config
      negateResult = evalBinaryOpIdentityConfigNegate config
      same = left == right
      result = if negateResult then not same else same
   in Right (IntValue {intValue = if result then 1 else 0})
