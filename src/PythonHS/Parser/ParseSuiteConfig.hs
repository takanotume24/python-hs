module PythonHS.Parser.ParseSuiteConfig (ParseSuiteConfig (..)) where

import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)

-- | Configuration for parsing a suite.
data ParseSuiteConfig = ParseSuiteConfig
  { parseSuiteStatement :: [Token] -> Either ParseError (Stmt, [Token]),
    parseSuiteTokenStream :: [Token]
  }
