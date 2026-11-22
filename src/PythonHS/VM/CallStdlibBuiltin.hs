{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

module PythonHS.VM.CallStdlibBuiltin (callStdlibBuiltin) where

import Control.Concurrent (threadDelay)
import Data.Time.Clock.POSIX (getPOSIXTime)
import PythonHS.Evaluator.ModuleAttr (ModuleAttr (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CallStdlibBuiltinConfig (CallStdlibBuiltinConfig (..))
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)

callStdlibBuiltin :: CallStdlibBuiltinConfig -> Maybe (Either String Value)
callStdlibBuiltin config =
  case name of
    "dumps" -> Just (evalJsonDumps args)
    "loads" -> Just (evalJsonLoads args)
    "Path" -> Just (evalPathlibPath args)
    "getcwd" -> Just (evalOsGetcwd args)
    "getattr" -> Just (evalGetattr args pos)
    "time" -> moduleCall "time" evalTime
    "sleep" -> moduleCall "time" evalSleep
    "randint" -> moduleCall "random" evalRandint
    "choice" -> moduleCall "random" evalChoice
    _ -> Nothing
  where
    name = normalizeName (callStdlibBuiltinName config)
    args = callStdlibBuiltinArgs config
    pos = callStdlibBuiltinPos config

    normalizeName rawName =
      case rawName of
        "__python_hs_stdlib_dumps" -> "dumps"
        "__python_hs_stdlib_loads" -> "loads"
        "__python_hs_stdlib_Path" -> "Path"
        "__python_hs_stdlib_getcwd" -> "getcwd"
        "__python_hs_stdlib_getattr" -> "getattr"
        "__python_hs_stdlib_time" -> "time"
        "__python_hs_stdlib_sleep" -> "sleep"
        "__python_hs_stdlib_randint" -> "randint"
        "__python_hs_stdlib_choice" -> "choice"
        _ -> rawName

    moduleCall expectedModule handler =
      case args of
        ModuleValue {moduleValueName = moduleName} : _
          | moduleName == expectedModule -> Just (handler args)
        _ -> Nothing

    evalTime values =
      case values of
        [ModuleValue {moduleValueName = moduleName}]
          | moduleName == "time" ->
              let now = unsafePerformIO getPOSIXTime
               in now `seq` Right (FloatValue {floatValue = realToFrac now})
        [_] -> Left ("Type error: time expects time module receiver at " ++ showPos pos)
        _ -> Left ("Argument count mismatch when calling time at " ++ showPos pos)

    evalSleep values =
      case values of
        [ModuleValue {moduleValueName = moduleName}, IntValue {intValue = seconds}]
          | moduleName == "time" -> sleepSeconds (fromIntegral seconds)
        [ModuleValue {moduleValueName = moduleName}, FloatValue {floatValue = seconds}]
          | moduleName == "time" -> sleepSeconds seconds
        [ModuleValue {}, _] -> Left ("Type error: sleep expects number at " ++ showPos pos)
        _ -> Left ("Argument count mismatch when calling sleep at " ++ showPos pos)

    sleepSeconds seconds
      | seconds < 0 = Left ("Value error: sleep length must be non-negative at " ++ showPos pos)
      | otherwise =
          let waited = unsafePerformIO (threadDelay (round (seconds * 1000000)) >> pure ())
           in waited `seq` Right NoneValue

    evalRandint values =
      case values of
        [ModuleValue {moduleValueName = moduleName}, IntValue {intValue = low}, IntValue {intValue = high}]
          | moduleName == "random" ->
              if low > high
                then Left ("Value error: empty range for randint at " ++ showPos pos)
                else
                  let picked = unsafePerformIO (randomRIO (low, high))
                   in picked `seq` Right (IntValue {intValue = picked})
        [ModuleValue {}, _, _] -> Left ("Type error: randint expects int arguments at " ++ showPos pos)
        _ -> Left ("Argument count mismatch when calling randint at " ++ showPos pos)

    evalChoice values =
      case values of
        [ModuleValue {moduleValueName = moduleName}, ListValue {listValueItems = items}]
          | moduleName == "random" -> chooseOne items
        [ModuleValue {moduleValueName = moduleName}, TupleValue {tupleValueItems = items}]
          | moduleName == "random" -> chooseOne items
        [ModuleValue {moduleValueName = moduleName}, StringValue {stringValue = text}]
          | moduleName == "random" -> chooseOne (fmap (\ch -> StringValue {stringValue = [ch]}) text)
        [ModuleValue {}, _] -> Left ("Type error: choice expects a non-empty sequence at " ++ showPos pos)
        _ -> Left ("Argument count mismatch when calling choice at " ++ showPos pos)

    chooseOne items =
      case items of
        [] -> Left ("Index error: cannot choose from an empty sequence at " ++ showPos pos)
        _ ->
          let index = unsafePerformIO (randomRIO (0, length items - 1))
           in index `seq` Right (items !! index)

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
              case filter (null . snd) (reads s) of
                [parsed] -> Right (IntValue {intValue = fst parsed})
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
              case filter (\attr -> moduleAttrName attr == attrName) attrs of
                (attr : _) -> Right (moduleAttrValue attr)
                [] -> Left ("Attribute error: module has no attribute '" ++ attrName ++ "' at " ++ showPos attrPos)
            _ -> Left ("Type error: getattr expects module object at " ++ showPos attrPos)
        [_, _] -> Left ("Type error: getattr expects string attribute name at " ++ showPos attrPos)
        _ -> Left ("Argument count mismatch when calling getattr at " ++ showPos attrPos)
