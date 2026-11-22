module PythonHS.VM.ExecutePrintTop (executePrintTop) where

import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Evaluator.ValueToOutput (valueToOutput)
import PythonHS.Lexer.Position (Position (..))
import PythonHS.VM.CallInstanceMethodString (callInstanceMethodString)
import PythonHS.VM.CallInstanceMethodStringConfig (CallInstanceMethodStringConfig (..))
import PythonHS.VM.CallInstanceMethodStringResult (CallInstanceMethodStringResult (..))
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.ExecutePrintTopConfig (ExecutePrintTopConfig (..))
import PythonHS.VM.VMState (VMState (..))

executePrintTop :: ExecutePrintTopConfig -> Either String VMState
executePrintTop config =
  case vmStack state of
    value : rest ->
      case callInstanceMethodString (methodConfig value) of
        Just (Left err) -> Left err
        Just (Right result) -> Right (stateAfterMethod state rest result)
        Nothing -> Right state {vmIp = vmIp state + 1, vmStack = rest, vmOutputs = vmOutputs state ++ [valueToOutput value]}
    _ -> Left "VM runtime error: print requires one value on stack"
  where
    state = executePrintTopState config

    methodConfig value =
      CallInstanceMethodStringConfig
        { callInstanceMethodStringExecute = executePrintTopExecute config,
          callInstanceMethodStringIsTopLevel = vmIsTopLevel state,
          callInstanceMethodStringPos = Position {line = 0, column = 0},
          callInstanceMethodStringStack = restOf state,
          callInstanceMethodStringInstance = value,
          callInstanceMethodStringGlobals = envGlobals (vmEnv state),
          callInstanceMethodStringLocals = envLocals (vmEnv state),
          callInstanceMethodStringFunctions = envFunctions (vmEnv state),
          callInstanceMethodStringOutputs = vmOutputs state
        }

    restOf currentState =
      case vmStack currentState of
        _ : remaining -> remaining
        [] -> []

    stateAfterMethod currentState rest result =
      currentState
        { vmIp = vmIp currentState + 1,
          vmStack = rest,
          vmEnv =
            (vmEnv currentState)
              { envGlobals = callInstanceMethodStringResultGlobals result,
                envLocals = callInstanceMethodStringResultLocals result,
                envFunctions = callInstanceMethodStringResultFunctions result
              },
          vmOutputs = callInstanceMethodStringResultOutputs result ++ [resultText result]
        }

    resultText result =
      case callInstanceMethodStringResultStack result of
        (StringValue {stringValue = text} : _) -> text
        _ -> ""
