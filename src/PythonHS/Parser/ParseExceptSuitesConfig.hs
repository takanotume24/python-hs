module PythonHS.Parser.ParseExceptSuitesConfig (ParseExceptSuitesConfig (..)) where

import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult)

data ParseExceptSuitesConfig = ParseExceptSuitesConfig
  { parseExceptSuitesSuite :: [Token] -> Either ParseError (ParseResult [Stmt]),
    parseExceptSuitesTokenStream :: [Token]
  }
