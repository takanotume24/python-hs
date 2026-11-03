module PythonHS.VM.CallExceptionClassBuiltin (callExceptionClassBuiltin) where

import PythonHS.Evaluator.ExceptionClassToPrefix (exceptionClassToPrefix)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CallExceptionClassBuiltinConfig (CallExceptionClassBuiltinConfig (..))

callExceptionClassBuiltin :: CallExceptionClassBuiltinConfig -> Maybe (Either String Value)
callExceptionClassBuiltin config =
  let className = callExceptionClassBuiltinName config
      args = callExceptionClassBuiltinArgs config
   in if exceptionClassToPrefix className == "Runtime error: " && className /= "RuntimeError"
        then Nothing
        else Just $ case args of
          [StringValue {stringValue = message}] ->
            Right ExceptionValue {exceptionValueClass = className, exceptionValueMessage = message}
          [] -> Right ExceptionValue {exceptionValueClass = className, exceptionValueMessage = ""}
          _ -> Left ("Type error: " ++ className ++ " accepts at most one string argument at " ++ showPos (callExceptionClassBuiltinPos config))
