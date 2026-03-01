module PythonHS.VM.FirstKeywordArg (firstKeywordArg) where

import PythonHS.VM.Instruction (Instruction)
import PythonHS.Lexer.Position (Position)

firstKeywordArg :: [([Instruction], Maybe String, Position)] -> Maybe (String, Position)
firstKeywordArg compiledArgs =
  case compiledArgs of
    [] -> Nothing
    (_, argKind, argPos) : restCompiledArgs ->
      case argKind of
        Nothing -> firstKeywordArg restCompiledArgs
        Just argName -> Just (argName, argPos)
