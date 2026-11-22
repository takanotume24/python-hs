module PythonHS.VM.EvalBinaryOpSet (evalBinaryOpSet) where

import PythonHS.AST.BinaryOperator (BinaryOperator (BitwiseAndOperator, BitwiseOrOperator, BitwiseXorOperator, SubtractOperator))
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Evaluator.ValuesEqual (valuesEqual)
import PythonHS.VM.EvalBinaryOpSetConfig (EvalBinaryOpSetConfig (..))

-- | Evaluate the set operators @|@, @&@, @^@ and @-@ when both operands are
-- sets. Returns 'Nothing' when the operator is not a set operation or when the
-- operands are not both sets, so the regular numeric operators can handle them.
evalBinaryOpSet :: EvalBinaryOpSetConfig -> Maybe (Either String Value)
evalBinaryOpSet config =
  case (evalBinaryOpSetLeft config, evalBinaryOpSetRight config) of
    (SetValue {setValueItems = leftItems, setValueIsFrozen = frozen}, SetValue {setValueItems = rightItems}) ->
      case evalBinaryOpSetOp config of
        BitwiseOrOperator -> Just (Right (makeSet (foldl insertUnique leftItems rightItems)))
        BitwiseAndOperator -> Just (Right (makeSet (filter (\item -> any (valuesEqual item) rightItems) leftItems)))
        BitwiseXorOperator -> Just (Right (makeSet (filter (\item -> not (any (valuesEqual item) rightItems)) leftItems ++ filter (\item -> not (any (valuesEqual item) leftItems)) rightItems)))
        SubtractOperator -> Just (Right (makeSet (filter (\item -> not (any (valuesEqual item) rightItems)) leftItems)))
        _ -> Nothing
      where
        makeSet items = SetValue {setValueItems = foldl insertUnique [] items, setValueIsFrozen = frozen}
    _ -> Nothing
  where
    insertUnique acc value =
      if any (valuesEqual value) acc then acc else acc ++ [value]
