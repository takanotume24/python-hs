module PythonHS.VM.CallStdlibBuiltin (callStdlibBuiltin) where

import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (IntValue, ModuleValue, StringValue), Value)
import PythonHS.Lexer.Position (Position)

callStdlibBuiltin :: String -> [Value] -> Position -> Maybe (Either String Value)
callStdlibBuiltin name args pos =
  case name of
    "dumps" -> Just (evalJsonDumps args)
    "loads" -> Just (evalJsonLoads args)
    "Path" -> Just (evalPathlibPath args)
    "getcwd" -> Just (evalOsGetcwd args)
    _ -> Nothing
  where
    evalJsonDumps values =
      case values of
        [ModuleValue moduleName _, IntValue n]
          | moduleName == "json" -> Right (StringValue (show n))
        [ModuleValue moduleName _, StringValue s]
          | moduleName == "json" -> Right (StringValue ("\"" ++ escapeJsonString s ++ "\""))
        [ModuleValue _ _, _] -> Left ("Type error: dumps expects int or string at " ++ showPos pos)
        [_, _] -> Left ("Type error: dumps expects json module receiver at " ++ showPos pos)
        _ -> Left ("Argument count mismatch when calling dumps at " ++ showPos pos)

    evalJsonLoads values =
      case values of
        [ModuleValue moduleName _, StringValue s]
          | moduleName == "json" ->
              case reads s of
                [(n, "")] -> Right (IntValue n)
                _ -> Left ("Value error: loads expects integer JSON literal at " ++ showPos pos)
        [ModuleValue _ _, _] -> Left ("Type error: loads expects string at " ++ showPos pos)
        [_, _] -> Left ("Type error: loads expects json module receiver at " ++ showPos pos)
        _ -> Left ("Argument count mismatch when calling loads at " ++ showPos pos)

    evalPathlibPath values =
      case values of
        [ModuleValue moduleName _, StringValue s]
          | moduleName == "pathlib" -> Right (StringValue s)
        [ModuleValue _ _, _] -> Left ("Type error: Path expects string at " ++ showPos pos)
        [_, _] -> Left ("Type error: Path expects pathlib module receiver at " ++ showPos pos)
        _ -> Left ("Argument count mismatch when calling Path at " ++ showPos pos)

    evalOsGetcwd values =
      case values of
        [ModuleValue moduleName _]
          | moduleName == "os" -> Right (StringValue ".")
        [_] -> Left ("Type error: getcwd expects os module receiver at " ++ showPos pos)
        _ -> Left ("Argument count mismatch when calling getcwd at " ++ showPos pos)

    escapeJsonString text =
      case text of
        [] -> []
        '"' : rest -> '\\' : '"' : escapeJsonString rest
        '\\' : rest -> '\\' : '\\' : escapeJsonString rest
        ch : rest -> ch : escapeJsonString rest
