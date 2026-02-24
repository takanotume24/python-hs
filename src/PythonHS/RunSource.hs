module PythonHS.RunSource (runSource) where

import PythonHS.EvalProgram (evalProgram)
import PythonHS.Lexer.ScanTokens (scanTokens)
import PythonHS.Parser.ParseProgram (parseProgram)

runSource :: String -> Either String [String]
runSource src =
  case scanTokens src of
    Left lexErr -> Left (show lexErr)
    Right tokens ->
      case parseProgram tokens of
        Left parseErr -> Left (show parseErr)
        Right prog -> evalProgram prog
