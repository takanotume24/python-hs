module PythonHS.Parser.ParseSingleParameterResult (ParseSingleParameterResult (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)

data ParseSingleParameterResult = ParseSingleParameterResult
  { parseSingleParameterResultName :: String,
    parseSingleParameterResultDefault :: Maybe Expr,
    parseSingleParameterResultPos :: Position
  }
  deriving (Eq, Show)
