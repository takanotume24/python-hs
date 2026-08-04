module PythonHS.VM.ApplyExceptionInstruction (applyExceptionInstruction) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.Value (Value (StringValue))
import PythonHS.VM.ExceptionApplyResult (ExceptionApplyResult (..))
import PythonHS.VM.ExecutePendingExceptionInstruction (executePendingExceptionInstruction)
import PythonHS.VM.Instruction (Instruction (LoadPendingException, MatchExceptionType, PopExceptionHandler, PushExceptionHandler, PushFinallyHandler, PushWithHandler, RaisePendingError, RaisePendingException))

applyExceptionInstruction :: Int -> Instruction -> [Value] -> Map.Map String Value -> [Int] -> Either String ExceptionApplyResult
applyExceptionInstruction ip instruction stack localEnv exceptionHandlers =
  case instruction of
    PushExceptionHandler handlerIp ->
      Right ExceptionApplyResult {exceptionApplyResultError = Nothing, exceptionApplyResultIp = ip + 1, exceptionApplyResultStack = stack, exceptionApplyResultHandlers = handlerIp : exceptionHandlers}
    PushFinallyHandler handlerIp ->
      Right ExceptionApplyResult {exceptionApplyResultError = Nothing, exceptionApplyResultIp = ip + 1, exceptionApplyResultStack = stack, exceptionApplyResultHandlers = encodeFinallyHandler handlerIp : exceptionHandlers}
    PushWithHandler handlerIp ->
      Right ExceptionApplyResult {exceptionApplyResultError = Nothing, exceptionApplyResultIp = ip + 1, exceptionApplyResultStack = stack, exceptionApplyResultHandlers = handlerIp : exceptionHandlers}
    PopExceptionHandler ->
      case exceptionHandlers of
        _ : restHandlers -> Right ExceptionApplyResult {exceptionApplyResultError = Nothing, exceptionApplyResultIp = ip + 1, exceptionApplyResultStack = stack, exceptionApplyResultHandlers = restHandlers}
        [] -> Left "Runtime error: attempting to pop from empty exception handler stack"
    RaisePendingError ->
      case Map.lookup pendingFinallyErrorName localEnv of
        Just (StringValue err) -> Right ExceptionApplyResult {exceptionApplyResultError = Just err, exceptionApplyResultIp = ip, exceptionApplyResultStack = stack, exceptionApplyResultHandlers = exceptionHandlers}
        _ -> Right ExceptionApplyResult {exceptionApplyResultError = Nothing, exceptionApplyResultIp = ip + 1, exceptionApplyResultStack = stack, exceptionApplyResultHandlers = exceptionHandlers}
    pendingInstruction@(LoadPendingException) -> runPending pendingInstruction
    pendingInstruction@(MatchExceptionType _) -> runPending pendingInstruction
    pendingInstruction@(RaisePendingException) -> runPending pendingInstruction
    _ -> Left "VM runtime error: applyExceptionInstruction called with non-exception instruction"
  where
    pendingFinallyErrorName = "__python_hs_pending_finally_error__"
    pendingExceptErrorName = "__python_hs_pending_except_error__"

    runPending pendingInstruction =
      case executePendingExceptionInstruction pendingExceptErrorName pendingInstruction stack localEnv of
        Left err -> Left err
        Right result -> Right ExceptionApplyResult {exceptionApplyResultError = fst result, exceptionApplyResultIp = ip + 1, exceptionApplyResultStack = snd result, exceptionApplyResultHandlers = exceptionHandlers}

    encodeFinallyHandler handlerIp = negate (handlerIp + 1)
