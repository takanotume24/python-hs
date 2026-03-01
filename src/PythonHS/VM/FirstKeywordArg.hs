module PythonHS.VM.FirstKeywordArg (firstKeywordArg) where

import PythonHS.Lexer.Position (Position)

firstKeywordArg :: [(Maybe String, Position)] -> Maybe (String, Position)
firstKeywordArg kinds =
  case kinds of
    [] -> Nothing
    (argKind, argPos) : restKinds ->
      case argKind of
        Nothing -> firstKeywordArg restKinds
        Just argName -> Just (argName, argPos)
