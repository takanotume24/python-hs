module PythonHS.Parser.ParseRaiseStmtConfig (ParseRaiseStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult)

data ParseRaiseStmtConfig = ParseRaiseStmtConfig
  { parseRaiseStmtExpr :: [Token] -> Either ParseError (ParseResult Expr),
    parseRaiseStmtPos :: Position,
    parseRaiseStmtTokenStream :: [Token]
  }
