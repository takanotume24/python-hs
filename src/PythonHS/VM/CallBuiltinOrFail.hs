module PythonHS.VM.CallBuiltinOrFail (callBuiltinOrFail) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CallBuiltin (callBuiltin)
import PythonHS.VM.CallBuiltinConfig (CallBuiltinConfig (..))
import PythonHS.VM.CallBuiltinOrFailConfig (CallBuiltinOrFailConfig (..))
import PythonHS.VM.CallExceptionClassBuiltin (callExceptionClassBuiltin)
import PythonHS.VM.CallExceptionClassBuiltinConfig (CallExceptionClassBuiltinConfig (..))
import PythonHS.VM.CallInstanceMethodStringConfig (CallInstanceMethodStringConfig (..))
import PythonHS.VM.CallInstanceMethodStringResult (CallInstanceMethodStringResult (..))
import PythonHS.VM.CallMapFilterBuiltin (callMapFilterBuiltin)
import PythonHS.VM.CallMapFilterBuiltinConfig (CallMapFilterBuiltinConfig (..))
import PythonHS.VM.CallMapFilterBuiltinResult (CallMapFilterBuiltinResult (..))
import PythonHS.VM.CallReflectionBuiltin (callReflectionBuiltin)
import PythonHS.VM.CallReflectionBuiltinConfig (CallReflectionBuiltinConfig (..))
import PythonHS.VM.CallStrConversion (callStrConversion)
import PythonHS.VM.CallStrConversionConfig (CallStrConversionConfig (..))
import PythonHS.VM.CallUnittestRuntime (callUnittestRuntime)
import PythonHS.VM.CallUnittestRuntimeConfig (CallUnittestRuntimeConfig (..))
import PythonHS.VM.ExecuteCallUserFunction (executeCallUserFunction)
import PythonHS.VM.FirstKeywordArg (firstKeywordArg)
import PythonHS.VM.FunctionInfo (FunctionInfo)
import PythonHS.VM.KeywordArgInfo (KeywordArgInfo (..))
import PythonHS.VM.ModulePrefixFor (modulePrefixFor)
import PythonHS.VM.SplitByDot (splitByDot)

callBuiltinOrFail :: CallBuiltinOrFailConfig -> Either String ([Value], Map.Map String Value, Map.Map String Value, Map.Map String FunctionInfo, [String])
callBuiltinOrFail config =
  case callMapFilterBuiltin mapFilterConfig of
    Just mapFilterResult ->
      case mapFilterResult of
        Left err -> Left err
        Right result ->
          Right
            ( callMapFilterBuiltinResultStack result,
              callMapFilterBuiltinResultGlobals result,
              callMapFilterBuiltinResultLocals result,
              callMapFilterBuiltinResultFunctions result,
              callMapFilterBuiltinResultOutputs result
            )
    Nothing ->
      case callExceptionClassBuiltin exceptionConfig of
        Just (Right exceptionValue) ->
          let newLocalEnv = if isTopLevel then globalsAfterArgs else localEnv
           in Right (exceptionValue : stack, globalsAfterArgs, newLocalEnv, functionsAfterArgs, outputsAfterArgs)
        Just (Left err) -> Left err
        Nothing ->
          case callModuleMemberFunction of
            Just result -> result
            Nothing ->
              case firstKeywordArg compiledArgs of
                Just kwInfo
                  | isBuiltinName name && name /= "subTest" ->
                      Left ("Argument error: keyword arguments are not supported for builtin " ++ name ++ " at " ++ showPos (keywordArgInfoPos kwInfo))
                Nothing ->
                  case callStrConversionFor of
                    Just conversionResult -> conversionResult
                    Nothing ->
                      case callUnittestRuntime runtimeConfig of
                        Just runtimeResult -> runtimeResult
                        Nothing -> dispatchBuiltin
                _ -> dispatchBuiltin
  where
    execute = callBuiltinOrFailExecute config
    isTopLevel = callBuiltinOrFailIsTopLevel config
    name = callBuiltinOrFailName config
    compiledArgs = callBuiltinOrFailCompiledArgs config
    pos = callBuiltinOrFailPos config
    stack = callBuiltinOrFailStack config
    localEnv = callBuiltinOrFailLocals config
    args = callBuiltinOrFailArgs config
    globalsAfterArgs = callBuiltinOrFailGlobals config
    functionsAfterArgs = callBuiltinOrFailFunctions config
    outputsAfterArgs = callBuiltinOrFailOutputs config

    mapFilterConfig =
      CallMapFilterBuiltinConfig
        { callMapFilterBuiltinExecute = execute,
          callMapFilterBuiltinIsTopLevel = isTopLevel,
          callMapFilterBuiltinName = name,
          callMapFilterBuiltinArgs = args,
          callMapFilterBuiltinPos = pos,
          callMapFilterBuiltinStack = stack,
          callMapFilterBuiltinGlobals = globalsAfterArgs,
          callMapFilterBuiltinLocals = localEnv,
          callMapFilterBuiltinFunctions = functionsAfterArgs,
          callMapFilterBuiltinOutputs = outputsAfterArgs
        }

    exceptionConfig =
      CallExceptionClassBuiltinConfig
        { callExceptionClassBuiltinName = name,
          callExceptionClassBuiltinArgs = args,
          callExceptionClassBuiltinPos = pos
        }

    dispatchBuiltin =
      case callReflectionBuiltin reflectionConfig of
        Just (Left err) -> Left err
        Just (Right builtinValue) -> pushBuiltin builtinValue
        Nothing ->
          case callBuiltin CallBuiltinConfig {callBuiltinName = name, callBuiltinArgs = args, callBuiltinPos = pos} of
            Just (Left err) -> Left err
            Just (Right builtinValue) -> pushBuiltin builtinValue
            Nothing -> Left ("Name error: undefined function " ++ name ++ " at " ++ showPos pos)

    pushBuiltin builtinValue =
      let newLocalEnv = if isTopLevel then globalsAfterArgs else localEnv
       in Right (builtinValue : stack, globalsAfterArgs, newLocalEnv, functionsAfterArgs, outputsAfterArgs)

    reflectionConfig =
      CallReflectionBuiltinConfig
        { callReflectionBuiltinName = name,
          callReflectionBuiltinArgs = args,
          callReflectionBuiltinPos = pos,
          callReflectionBuiltinGlobals = globalsAfterArgs,
          callReflectionBuiltinLocals = localEnv,
          callReflectionBuiltinFunctions = functionsAfterArgs
        }

    runtimeConfig =
      CallUnittestRuntimeConfig
        { callUnittestRuntimeExecute = execute,
          callUnittestRuntimeIsTopLevel = isTopLevel,
          callUnittestRuntimeName = name,
          callUnittestRuntimeArgs = args,
          callUnittestRuntimePos = pos,
          callUnittestRuntimeStack = stack,
          callUnittestRuntimeGlobals = globalsAfterArgs,
          callUnittestRuntimeLocals = localEnv,
          callUnittestRuntimeFunctions = functionsAfterArgs,
          callUnittestRuntimeOutputs = outputsAfterArgs
        }

    callStrConversionFor =
      case callStrConversion
        CallStrConversionConfig
          { callStrConversionName = name,
            callStrConversionArgs = args,
            callStrConversionMethodConfig =
              CallInstanceMethodStringConfig
                { callInstanceMethodStringExecute = execute,
                  callInstanceMethodStringIsTopLevel = isTopLevel,
                  callInstanceMethodStringPos = pos,
                  callInstanceMethodStringStack = stack,
                  callInstanceMethodStringInstance = IntValue 0,
                  callInstanceMethodStringGlobals = globalsAfterArgs,
                  callInstanceMethodStringLocals = localEnv,
                  callInstanceMethodStringFunctions = functionsAfterArgs,
                  callInstanceMethodStringOutputs = outputsAfterArgs
                }
          } of
        Nothing -> Nothing
        Just (Left err) -> Just (Left err)
        Just (Right result) ->
          Just
            ( Right
                ( callInstanceMethodStringResultStack result,
                  callInstanceMethodStringResultGlobals result,
                  callInstanceMethodStringResultLocals result,
                  callInstanceMethodStringResultFunctions result,
                  callInstanceMethodStringResultOutputs result
                )
            )

    callModuleMemberFunction =
      case args of
        ModuleValue moduleName _ : restArgs ->
          let memberFunctionName = modulePrefixFor (splitByDot moduleName) ++ name
           in case Map.lookup memberFunctionName functionsAfterArgs of
                Just _ -> Just (executeCallUserFunction execute isTopLevel pos stack localEnv memberFunctionName Nothing restArgs globalsAfterArgs functionsAfterArgs outputsAfterArgs)
                Nothing -> Nothing
        _ -> Nothing

    isBuiltinName builtinName =
      builtinName `elem` ["type", "isinstance", "hasattr"]
        || case callBuiltin CallBuiltinConfig {callBuiltinName = builtinName, callBuiltinArgs = [], callBuiltinPos = pos} of
          Just _ -> True
          Nothing -> False
