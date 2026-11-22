module PythonHS.VM.EvalBinaryOpMembership (evalBinaryOpMembership) where

import Data.List (isInfixOf)
import PythonHS.Evaluator.DictPair (DictPair (DictPair))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Evaluator.ValuesEqual (valuesEqual)
import PythonHS.VM.EvalBinaryOpMembershipConfig (EvalBinaryOpMembershipConfig (..))

evalBinaryOpMembership :: EvalBinaryOpMembershipConfig -> Either String Value
evalBinaryOpMembership config =
  let left = evalBinaryOpMembershipConfigLeft config
      right = evalBinaryOpMembershipConfigRight config
      pos = evalBinaryOpMembershipConfigPos config
      negateResult = evalBinaryOpMembershipConfigNegate config
   in fmap (applyNegate negateResult) $ case right of
        ListValue {listValueItems = items} -> Right (boolValue (any (valuesEqual left) items))
        TupleValue {tupleValueItems = items} -> Right (boolValue (any (valuesEqual left) items))
        DictValue {dictValuePairs = pairs} -> Right (boolValue (any (valuesEqual left . pairKey) pairs))
        StringValue {stringValue = s} ->
          case left of
            StringValue {stringValue = t} -> Right (boolValue (t `isInfixOf` s))
            _ -> Left ("Type error: expected string in in at " ++ showPos pos)
        _ -> Left ("Type error: expected list/dict/tuple/string in in at " ++ showPos pos)
  where
    boolValue condition = IntValue {intValue = if condition then 1 else 0}

    applyNegate True IntValue {intValue = n} = IntValue {intValue = if n == 0 then 1 else 0}
    applyNegate True _ = IntValue {intValue = 0}
    applyNegate False value = value

    pairKey :: DictPair k v -> k
    pairKey (DictPair key _) = key
