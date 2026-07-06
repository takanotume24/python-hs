module PythonHS.Parser.ParseParametersResult (ParseParametersResult (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.FunctionDefault (FunctionDefault)

data ParseParametersResult = ParseParametersResult
  { parseParametersResultParams :: [String],
    parseParametersResultDefaults :: [FunctionDefault Expr]
  }
  deriving (Eq, Show)
