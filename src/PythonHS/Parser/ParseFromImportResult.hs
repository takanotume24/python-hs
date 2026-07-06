module PythonHS.Parser.ParseFromImportResult (ParseFromImportResult (..)) where

import PythonHS.Lexer.Token (Token)

data ParseFromImportResult = ParseFromImportResult
  { parseFromImportResultLevel :: Int,
    parseFromImportResultModulePath :: [String],
    parseFromImportResultRemaining :: [Token]
  }
