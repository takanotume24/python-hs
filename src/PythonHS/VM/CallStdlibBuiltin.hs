module PythonHS.VM.CallStdlibBuiltin (callStdlibBuiltin) where

import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CallStdlibBuiltinConfig (CallStdlibBuiltinConfig (..))

callStdlibBuiltin :: CallStdlibBuiltinConfig -> Maybe (Either String Value)
callStdlibBuiltin config = case name of
  "dumps" -> Just (evalJsonDumps args)
  "loads" -> Just (evalJsonLoads args)
  "Path" -> Just (evalPathlibPath args)
  "getcwd" -> Just (evalOsGetcwd args)
  "getattr" -> Just (evalGetattr args pos)
  _ -> Nothing
  where
    name = callStdlibBuiltinName config
    args = callStdlibBuiltinArgs config
    pos = callStdlibBuiltinPos config
    evalJsonDumps values =
      case values of
        [ModuleValue {moduleValueName = moduleName}, IntValue {intValue = n}]
          | moduleName == "json" -> Right (StringValue {stringValue = show n})
        [ModuleValue {moduleValueName = moduleName}, StringValue {stringValue = s}]
          | moduleName == "json" -> Right (StringValue {stringValue = "\"" ++ escapeJsonString s ++ "\""})
        [ModuleValue {}, _] -> Left ("Type error: dumps expects int or string at " ++ showPos pos)
        [_, _] -> Left ("Type error: dumps expects json module receiver at " ++ showPos pos)
        _ -> Left ("Argument count mismatch when calling dumps at " ++ showPos pos)

    evalJsonLoads values =
      case values of
        [ModuleValue {moduleValueName = moduleName}, StringValue {stringValue = s}]
          | moduleName == "json" ->
              case reads s of
                [(n, "")] -> Right (IntValue {intValue = n})
                _ -> Left ("Value error: loads expects integer JSON literal at " ++ showPos pos)
        [ModuleValue {}, _] -> Left ("Type error: loads expects string at " ++ showPos pos)
        [_, _] -> Left ("Type error: loads expects json module receiver at " ++ showPos pos)
        _ -> Left ("Argument count mismatch when calling loads at " ++ showPos pos)

    evalPathlibPath values =
      case values of
        [ModuleValue {moduleValueName = moduleName}, StringValue {stringValue = s}]
          | moduleName == "pathlib" -> Right (StringValue {stringValue = s})
        [ModuleValue {}, _] -> Left ("Type error: Path expects string at " ++ showPos pos)
        [_, _] -> Left ("Type error: Path expects pathlib module receiver at " ++ showPos pos)
        _ -> Left ("Argument count mismatch when calling Path at " ++ showPos pos)

    evalOsGetcwd values =
      case values of
        [ModuleValue {moduleValueName = moduleName}]
          | moduleName == "os" -> Right (StringValue {stringValue = "."})
        [_] -> Left ("Type error: getcwd expects os module receiver at " ++ showPos pos)
        _ -> Left ("Argument count mismatch when calling getcwd at " ++ showPos pos)

    escapeJsonString text =
      case text of
        [] -> []
        '"' : rest -> '\\' : '"' : escapeJsonString rest
        '\\' : rest -> '\\' : '\\' : escapeJsonString rest
        ch : rest -> ch : escapeJsonString rest

    evalGetattr values attrPos =
      case values of
        [obj, StringValue {stringValue = attrName}] ->
          case obj of
            ModuleValue {moduleValueAttrs = attrs} ->
              case lookup attrName attrs of
                Just value -> Right value
                Nothing -> Left ("Attribute error: module has no attribute '" ++ attrName ++ "' at " ++ showPos attrPos)
            _ -> Left ("Type error: getattr expects module object at " ++ showPos attrPos)
        [_, _] -> Left ("Type error: getattr expects string attribute name at " ++ showPos attrPos)
        _ -> Left ("Argument count mismatch when calling getattr at " ++ showPos attrPos)
