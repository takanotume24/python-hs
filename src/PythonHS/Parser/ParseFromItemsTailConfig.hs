module PythonHS.Parser.ParseFromItemsTailConfig (ParseFromItemsTailConfig (..)) where

import PythonHS.AST.FromImportEntry (FromImportEntry)
import PythonHS.Lexer.Token (Token)

data ParseFromItemsTailConfig = ParseFromItemsTailConfig
  { parseFromItemsTailItems :: [FromImportEntry],
    parseFromItemsTailTokenStream :: [Token]
  }
