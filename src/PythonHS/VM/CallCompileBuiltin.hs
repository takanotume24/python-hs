module PythonHS.VM.CallCompileBuiltin (callCompileBuiltin) where

import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Lexer.ScanTokens (scanTokens)
import PythonHS.Parser.ParseProgram (parseProgram)
import PythonHS.VM.CallBuiltinConfig (CallBuiltinConfig (..))

-- | Minimal @compile(source, filename, mode)@ support.
--
-- The resulting value is a placeholder because python-hs does not model code
-- objects yet. Calling @compile@ is still useful: it validates syntax and
-- raises a catchable @SyntaxError@ for invalid source.
callCompileBuiltin :: CallBuiltinConfig -> Maybe (Either String Value)
callCompileBuiltin config =
  case callBuiltinName config of
    "compile" -> Just (evalCompile (callBuiltinArgs config))
    _ -> Nothing
  where
    pos = callBuiltinPos config

    evalCompile values =
      case values of
        [StringValue {stringValue = source}, StringValue {}, StringValue {}] ->
          case scanTokens source of
            Left lexerError -> syntaxError (show lexerError)
            Right tokens ->
              case parseProgram tokens of
                Left parseError -> syntaxError (show parseError)
                Right _ -> Right NoneValue
        _ -> Left ("Argument count mismatch when calling compile at " ++ showPos pos)

    syntaxError message =
      Left ("Syntax error: " ++ message ++ " at " ++ showPos pos)
