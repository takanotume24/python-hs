module PythonHS.Parser.ParseExceptSuitesConfig (ParseExceptSuitesConfig (..)) where

import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)

data ParseExceptSuitesConfig = ParseExceptSuitesConfig
  { parseExceptSuitesSuite :: [Token] -> Either ParseError ([Stmt], [Token]),
    parseExceptSuitesTokenStream :: [Token]
  }
