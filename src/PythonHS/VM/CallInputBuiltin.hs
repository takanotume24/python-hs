{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

-- | The @input()@ builtin reads one line from process standard input.
--
-- The VM executor is pure, so this is the single place where the interpreter
-- intentionally steps into IO. CSE and full-laziness are disabled to make sure
-- every @input()@ call performs a fresh read instead of being shared.
module PythonHS.VM.CallInputBuiltin (callInputBuiltin) where

import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CallBuiltinConfig (CallBuiltinConfig (..))
import System.IO (isEOF)
import System.IO.Unsafe (unsafePerformIO)

callInputBuiltin :: CallBuiltinConfig -> Maybe (Either String Value)
callInputBuiltin config =
  case callBuiltinName config of
    "input" -> Just (evalInput (callBuiltinArgs config) (callBuiltinPos config))
    _ -> Nothing
  where
    evalInput values pos =
      case values of
        [] -> readLine pos
        [StringValue {}] -> readLine pos
        [_] -> Left ("Type error: input expects string prompt at " ++ showPos pos)
        _ -> Left ("Argument count mismatch when calling input at " ++ showPos pos)

    readLine pos =
      case readStdinLine of
        Nothing -> Left ("Runtime error: EOF when reading a line at " ++ showPos pos)
        Just line -> Right (StringValue {stringValue = line})

    readStdinLine :: Maybe String
    readStdinLine = unsafePerformIO $ do
      endOfInput <- isEOF
      if endOfInput
        then pure Nothing
        else Just <$> getLine
