module PythonHS.Parser.ParsePatternConfig (ParsePatternConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult)

data ParsePatternConfig = ParsePatternConfig
  { parsePatternExpr :: [Token] -> Either ParseError (ParseResult Expr),
    parsePatternTokenStream :: [Token]
  }
