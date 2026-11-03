module PythonHS.VM.RaiseTopError (raiseTopError) where

import PythonHS.Evaluator.ExceptionClassToPrefix (exceptionClassToPrefix)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Evaluator.ValueToOutput (valueToOutput)
import PythonHS.VM.RaiseTopErrorConfig (RaiseTopErrorConfig (..))

raiseTopError :: RaiseTopErrorConfig -> String
raiseTopError config =
  let value = raiseTopErrorValue config
      pos = raiseTopErrorPos config
   in case value of
        ExceptionValue {exceptionValueClass = className, exceptionValueMessage = message} ->
          exceptionClassToPrefix className ++ message ++ " at " ++ showPos pos
        _ -> "Runtime error: " ++ valueToOutput value ++ " at " ++ showPos pos
