module PythonHS.AST.ExceptSuite (ExceptSuite (..)) where

import PythonHS.Lexer.Position (Position)

data ExceptSuite s = ExceptSuite
  { exceptSuiteType :: Maybe String,
    exceptSuiteAlias :: Maybe String,
    exceptSuiteBody :: [s],
    exceptSuitePos :: Position
  }
  deriving (Eq, Show)
