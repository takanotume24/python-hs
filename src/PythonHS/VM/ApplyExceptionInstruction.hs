module PythonHS.VM.ApplyExceptionInstruction (applyExceptionInstruction) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.Value (Value (IntValue, StringValue))
import PythonHS.VM.DropToLoopBoundary (dropToLoopBoundary)
import PythonHS.VM.EncodeFinallyHandler (encodeFinallyHandler)
import PythonHS.VM.ExceptionApplyResult (ExceptionApplyResult (..))
import PythonHS.VM.ExecutePendingExceptionInstruction (executePendingExceptionInstruction)
import PythonHS.VM.ExitKind (ExitKind (..))
import PythonHS.VM.Instruction (Instruction (..))
import PythonHS.VM.PendingControlKindName (pendingControlKindName)
import PythonHS.VM.PendingControlOwnerName (pendingControlOwnerName)
import PythonHS.VM.PendingControlTargetName (pendingControlTargetName)
import PythonHS.VM.PendingFinallyErrorName (pendingFinallyErrorName)
import PythonHS.VM.PendingFinallyOwnerName (pendingFinallyOwnerName)
import PythonHS.VM.PendingReturnValueName (pendingReturnValueName)
import PythonHS.VM.TakeFinallyHandler (takeFinallyHandler)

applyExceptionInstruction :: Int -> Instruction -> [Value] -> Map.Map String Value -> [Int] -> Either String ExceptionApplyResult
applyExceptionInstruction ip instruction stack localEnv exceptionHandlers =
  case instruction of
    PushExceptionHandler handlerIp ->
      Right result {exceptionApplyResultIp = ip + 1, exceptionApplyResultHandlers = handlerIp : exceptionHandlers}
    PushFinallyHandler handlerIp ->
      Right result {exceptionApplyResultIp = ip + 1, exceptionApplyResultHandlers = encodeFinallyHandler handlerIp : exceptionHandlers}
    PushWithHandler handlerIp ->
      Right result {exceptionApplyResultIp = ip + 1, exceptionApplyResultHandlers = handlerIp : exceptionHandlers}
    PopExceptionHandler ->
      case exceptionHandlers of
        _ : restHandlers -> Right result {exceptionApplyResultIp = ip + 1, exceptionApplyResultHandlers = restHandlers}
        [] -> Left "Runtime error: attempting to pop from empty exception handler stack"
    RaisePendingError {raisePendingErrorFinallyStart = finallyStart} ->
      case (Map.lookup pendingFinallyErrorName localEnv, Map.lookup pendingFinallyOwnerName localEnv) of
        (Just (StringValue err), Just (IntValue ownerIp))
          | fromIntegral ownerIp == finallyStart -> Right result {exceptionApplyResultError = Just err}
        _ -> resumePendingControl finallyStart
    pendingInstruction@(LoadPendingException) -> runPending pendingInstruction
    pendingInstruction@LoadPendingExceptionClass -> runPending pendingInstruction
    pendingInstruction@LoadPendingExceptionMessage -> runPending pendingInstruction
    pendingInstruction@(MatchExceptionType _) -> runPending pendingInstruction
    pendingInstruction@(RaisePendingException) -> runPending pendingInstruction
    _ -> Left "VM runtime error: applyExceptionInstruction called with non-exception instruction"
  where
    pendingExceptErrorName = "__python_hs_pending_except_error__"

    result =
      ExceptionApplyResult
        { exceptionApplyResultError = Nothing,
          exceptionApplyResultIp = ip,
          exceptionApplyResultStack = stack,
          exceptionApplyResultHandlers = exceptionHandlers,
          exceptionApplyResultLocalChanges = []
        }

    runPending pendingInstruction =
      case executePendingExceptionInstruction pendingExceptErrorName pendingInstruction stack localEnv of
        Left err -> Left err
        Right (maybeError, newStack) ->
          Right result {exceptionApplyResultError = maybeError, exceptionApplyResultIp = ip + 1, exceptionApplyResultStack = newStack}

    resumePendingControl finallyStart =
      case (Map.lookup pendingControlKindName localEnv, Map.lookup pendingControlOwnerName localEnv) of
        (Just (StringValue controlKind), Just (IntValue ownerIp))
          | fromIntegral ownerIp == finallyStart -> resume controlKind
        _ -> Right result {exceptionApplyResultIp = ip + 1}

    resume controlKind =
      case controlKind of
        "return" -> resumeReturn
        "break" -> resumeLoop ExitBreak
        "continue" -> resumeLoop ExitContinue
        _ -> Right result {exceptionApplyResultIp = ip + 1}

    resumeReturn =
      case takeFinallyHandler False exceptionHandlers of
        Just (restHandlers, nextFinallyStart) -> runNextFinally nextFinallyStart restHandlers
        Nothing ->
          let returnValue = case Map.lookup pendingReturnValueName localEnv of
                Just value -> [value]
                Nothing -> []
           in Right result {exceptionApplyResultIp = -1, exceptionApplyResultStack = returnValue, exceptionApplyResultHandlers = [], exceptionApplyResultLocalChanges = clearControlChanges}

    resumeLoop exitKind =
      case takeFinallyHandler True exceptionHandlers of
        Just (restHandlers, nextFinallyStart) -> runNextFinally nextFinallyStart restHandlers
        Nothing ->
          case Map.lookup pendingControlTargetName localEnv of
            Just (IntValue targetIp) ->
              let includeBoundary = exitKind == ExitBreak
               in Right
                    result
                      { exceptionApplyResultIp = fromIntegral targetIp,
                        exceptionApplyResultHandlers = dropToLoopBoundary includeBoundary exceptionHandlers,
                        exceptionApplyResultLocalChanges = clearControlChanges
                      }
            _ -> Right result {exceptionApplyResultIp = ip + 1}

    runNextFinally nextFinallyStart restHandlers =
      Right
        result
          { exceptionApplyResultIp = nextFinallyStart + 1,
            exceptionApplyResultHandlers = restHandlers,
            exceptionApplyResultLocalChanges = [(pendingControlOwnerName, Just (IntValue (fromIntegral nextFinallyStart)))]
          }

    clearControlChanges =
      map (\name -> (name, Nothing)) [pendingControlKindName, pendingControlTargetName, pendingControlOwnerName, pendingReturnValueName]
