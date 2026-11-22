module PythonHS.VM.CallFileBuiltin (callFileBuiltin) where

import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CallFileBuiltinConfig (CallFileBuiltinConfig (..))
import PythonHS.VM.CallFileBuiltinResult (CallFileBuiltinResult (..))
import PythonHS.VM.PropagateInstanceMutation (propagateInstanceMutation)
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)

-- | Minimal text file I/O. A file object is represented as an
-- @InstanceValue \"__File__\"@ carrying its path, mode, and closed flag.
callFileBuiltin :: CallFileBuiltinConfig -> Maybe (Either String CallFileBuiltinResult)
callFileBuiltin config =
  case callFileBuiltinName config of
    "open" -> Just (evalOpen args)
    "read" -> method evalRead
    "write" -> method evalWrite
    "close" -> method evalClose
    "__enter__" -> method evalEnter
    "__exit__" -> method evalExit
    _ -> Nothing
  where
    args = callFileBuiltinArgs config
    pos = callFileBuiltinPos config
    isTopLevel = callFileBuiltinIsTopLevel config
    globals = callFileBuiltinGlobals config
    locals = callFileBuiltinLocals config

    evalOpen values =
      case values of
        [StringValue {stringValue = path}] -> openWith path "r"
        [StringValue {stringValue = path}, StringValue {stringValue = mode}] -> openWith path mode
        [_, _] -> Left ("Type error: open expects string path and mode at " ++ showPos pos)
        [_] -> Left ("Type error: open expects string path at " ++ showPos pos)
        _ -> Left ("Argument count mismatch when calling open at " ++ showPos pos)

    openWith path mode
      | mode == "r" =
          if doesFileExistNow path
            then Right (pushValue (fileObject path mode))
            else Left ("Runtime error: file not found: " ++ path ++ " at " ++ showPos pos)
      | mode == "w" =
          let created = unsafePerformIO (writeFile path "" >> pure True)
           in created `seq` Right (pushValue (fileObject path mode))
      | mode == "a" =
          let created = unsafePerformIO (appendFile path "" >> pure True)
           in created `seq` Right (pushValue (fileObject path mode))
      | otherwise = Left ("Value error: invalid mode '" ++ mode ++ "' at " ++ showPos pos)

    method handler =
      case args of
        fileValue@InstanceValue {instanceValueClass = "__File__"} : rest ->
          Just (handler fileValue rest)
        _ -> Nothing

    evalRead fileValue restArgs =
      case restArgs of
        [] -> do
          requireOpen fileValue
          mode <- requireMode fileValue
          if mode /= "r"
            then Left ("Runtime error: file not open for reading at " ++ showPos pos)
            else
              let contents = unsafePerformIO (readFile (filePath fileValue))
               in Right (pushValue (StringValue {stringValue = contents}))
        _ -> Left ("Argument count mismatch when calling read at " ++ showPos pos)

    evalWrite fileValue restArgs =
      case restArgs of
        [StringValue {stringValue = text}] -> do
          requireOpen fileValue
          mode <- requireMode fileValue
          if mode `notElem` ["w", "a"]
            then Left ("Runtime error: file not open for writing at " ++ showPos pos)
            else
              let written = unsafePerformIO (appendFile (filePath fileValue) text >> pure (length text))
               in written `seq` Right (pushValue (IntValue {intValue = fromIntegral written}))
        [_] -> Left ("Type error: write expects string at " ++ showPos pos)
        _ -> Left ("Argument count mismatch when calling write at " ++ showPos pos)

    evalClose fileValue _ = Right (pushClosed NoneValue fileValue)

    evalEnter fileValue _ = Right (pushValue fileValue)

    evalExit fileValue _ = Right (pushClosed (IntValue {intValue = 0}) fileValue)

    requireOpen fileValue =
      if isClosed fileValue
        then Left ("Runtime error: I/O operation on closed file at " ++ showPos pos)
        else Right ()

    requireMode fileValue =
      case lookup "mode" (instanceValueAttrs fileValue) of
        Just (StringValue {stringValue = mode}) -> Right mode
        _ -> Left ("Runtime error: invalid file object at " ++ showPos pos)

    pushValue resultValue =
      CallFileBuiltinResult
        { callFileBuiltinResultStack = resultValue : callFileBuiltinStack config,
          callFileBuiltinResultGlobals = globals,
          callFileBuiltinResultLocals = if isTopLevel then globals else locals,
          callFileBuiltinResultFunctions = callFileBuiltinFunctions config,
          callFileBuiltinResultOutputs = callFileBuiltinOutputs config
        }

    pushClosed resultValue fileValue =
      let closedValue = setClosed fileValue
          newGlobals = propagateInstanceMutation fileValue closedValue globals
          newLocals = propagateInstanceMutation fileValue closedValue locals
       in CallFileBuiltinResult
            { callFileBuiltinResultStack = resultValue : callFileBuiltinStack config,
              callFileBuiltinResultGlobals = newGlobals,
              callFileBuiltinResultLocals = if isTopLevel then newGlobals else newLocals,
              callFileBuiltinResultFunctions = callFileBuiltinFunctions config,
              callFileBuiltinResultOutputs = callFileBuiltinOutputs config
            }

    filePath fileValue =
      case lookup "path" (instanceValueAttrs fileValue) of
        Just (StringValue {stringValue = path}) -> path
        _ -> ""

    isClosed fileValue =
      case lookup "closed" (instanceValueAttrs fileValue) of
        Just (IntValue {intValue = n}) -> n /= 0
        _ -> False

    setClosed fileValue =
      case fileValue of
        InstanceValue {instanceValueClass = className, instanceValueAttrs = attrs} ->
          InstanceValue
            { instanceValueClass = className,
              instanceValueAttrs = replaceAttr "closed" (IntValue {intValue = 1}) attrs
            }
        otherValue -> otherValue

    replaceAttr key value attrs =
      case attrs of
        [] -> [(key, value)]
        (existingKey, existingValue) : rest
          | existingKey == key -> (key, value) : rest
          | otherwise -> (existingKey, existingValue) : replaceAttr key value rest

    fileObject path mode =
      InstanceValue
        { instanceValueClass = "__File__",
          instanceValueAttrs =
            [ ("path", StringValue {stringValue = path}),
              ("mode", StringValue {stringValue = mode}),
              ("closed", IntValue {intValue = 0})
            ]
        }

    doesFileExistNow path = unsafePerformIO (doesFileExist path)
