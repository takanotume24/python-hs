module PythonHS.VM.ApplyExceptionInstruction (applyExceptionInstruction) where

import qualified Data.Map.Strict as Map
import PythonHS.Evaluator.Value (Value (StringValue), Value)
import PythonHS.VM.ExecutePendingExceptionInstruction (executePendingExceptionInstruction)
import PythonHS.VM.Instruction (Instruction (LoadPendingException, MatchExceptionType, PopExceptionHandler, PushExceptionHandler, PushFinallyHandler, RaisePendingError, RaisePendingException))

applyExceptionInstruction :: Int -> Instruction -> [Value] -> Map.Map String Value -> [Int] -> Either String (Maybe String, Int, [Value], [Int])
applyExceptionInstruction ip instruction stack localEnv exceptionHandlers =
  case instruction of
    PushExceptionHandler handlerIp ->
      Right (Nothing, ip + 1, stack, handlerIp : exceptionHandlers)
    PushFinallyHandler handlerIp ->
      Right (Nothing, ip + 1, stack, encodeFinallyHandler handlerIp : exceptionHandlers)
    PopExceptionHandler ->
      case exceptionHandlers of
        _ : restHandlers -> Right (Nothing, ip + 1, stack, restHandlers)
        [] -> Right (Nothing, ip + 1, stack, [])
    RaisePendingError ->
      case Map.lookup pendingFinallyErrorName localEnv of
        Just (StringValue err) -> Right (Just err, ip, stack, exceptionHandlers)
        _ -> Right (Nothing, ip + 1, stack, exceptionHandlers)
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
        Right (maybeErr, newStack) -> Right (maybeErr, ip + 1, newStack, exceptionHandlers)

    encodeFinallyHandler handlerIp = negate (handlerIp + 1)
