module PythonHS.Evaluator.EvalExprBinaryMembership (evalExprBinaryMembership) where

import Data.List (isInfixOf)
import PythonHS.Evaluator.DictPair (DictPair (DictPair))
import PythonHS.Evaluator.EvalExprBinaryMembershipConfig (EvalExprBinaryMembershipConfig (..))
import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Lexer.Position (Position)

evalExprBinaryMembership :: EvalExprBinaryMembershipConfig -> Either String EvalExprResult
evalExprBinaryMembership config = do
  let evalExprFn = evalExprBinaryMembershipConfigEvalExprFn config
      env = evalExprBinaryMembershipConfigEnv config
      fenv = evalExprBinaryMembershipConfigFuncEnv config
      leftExpr = evalExprBinaryMembershipConfigLeftExpr config
      rightExpr = evalExprBinaryMembershipConfigRightExpr config
      pos = evalExprBinaryMembershipConfigPos config
      negateResult = evalExprBinaryMembershipConfigNegate config
  EvalExprResult {evalExprResultValue = leftVal, evalExprResultOutputs = leftOuts, evalExprResultEnv = envAfterLeft} <- evalExprFn env fenv leftExpr
  EvalExprResult {evalExprResultValue = rightVal, evalExprResultOutputs = rightOuts, evalExprResultEnv = envAfterRight} <- evalExprFn envAfterLeft fenv rightExpr
  membershipValue <- computeMembership pos leftVal rightVal
  let resultValue = applyNegate negateResult membershipValue
  Right (EvalExprResult {evalExprResultValue = resultValue, evalExprResultOutputs = leftOuts ++ rightOuts, evalExprResultEnv = envAfterRight})
  where
    computeMembership :: Position -> Value -> Value -> Either String Value
    computeMembership pos leftVal rightVal =
      case rightVal of
        ListValue {listValueItems = items} -> Right (boolValue (any (valuesEqual leftVal) items))
        TupleValue {tupleValueItems = items} -> Right (boolValue (any (valuesEqual leftVal) items))
        DictValue {dictValuePairs = pairs} -> Right (boolValue (any (valuesEqual leftVal . pairKey) pairs))
        StringValue {stringValue = s} ->
          case leftVal of
            StringValue {stringValue = t} -> Right (boolValue (t `isInfixOf` s))
            _ -> Left ("Type error: expected string in in at " ++ showPos pos)
        _ -> Left ("Type error: expected list/dict/tuple/string in in at " ++ showPos pos)
      where
        boolValue condition = IntValue {intValue = if condition then 1 else 0}

    pairKey :: DictPair k v -> k
    pairKey (DictPair key _) = key

    valuesEqual :: Value -> Value -> Bool
    valuesEqual (IntValue {intValue = leftInt}) (IntValue {intValue = rightInt}) = leftInt == rightInt
    valuesEqual (IntValue {intValue = leftInt}) (FloatValue {floatValue = rightFloat}) = (fromIntegral leftInt :: Double) == rightFloat
    valuesEqual (FloatValue {floatValue = leftFloat}) (IntValue {intValue = rightInt}) = leftFloat == (fromIntegral rightInt :: Double)
    valuesEqual (FloatValue {floatValue = leftFloat}) (FloatValue {floatValue = rightFloat}) = leftFloat == rightFloat
    valuesEqual (StringValue {stringValue = leftString}) (StringValue {stringValue = rightString}) = leftString == rightString
    valuesEqual (ListValue {listValueItems = leftItems}) (ListValue {listValueItems = rightItems}) = leftItems == rightItems
    valuesEqual (TupleValue {tupleValueItems = leftItems}) (TupleValue {tupleValueItems = rightItems}) = leftItems == rightItems
    valuesEqual leftValue rightValue = leftValue == rightValue

    applyNegate :: Bool -> Value -> Value
    applyNegate True IntValue {intValue = n} = IntValue {intValue = if n == 0 then 1 else 0}
    applyNegate True _ = IntValue {intValue = 0}
    applyNegate False value = value
