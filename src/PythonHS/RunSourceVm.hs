module PythonHS.RunSourceVm (runSourceVm) where

import PythonHS.Lexer.ScanTokens (scanTokens)
import PythonHS.Parser.ParseProgram (parseProgram)
import PythonHS.VM.CompileProgram (compileProgram)
import PythonHS.VM.RunInstructions (runInstructions)

runSourceVm :: String -> Either String [String]
runSourceVm src =
  case scanTokens src of
    Left lexErr -> Left (show lexErr)
    Right tokens ->
      case parseProgram tokens of
        Left parseErr -> Left (show parseErr)
        Right program ->
          case compileProgram program of
            Left compileErr -> Left compileErr
            Right instructions -> runInstructions instructions
