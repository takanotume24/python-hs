module PythonHS.Parser.ParseDelStmtConfig (ParseDelStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult)

data ParseDelStmtConfig = ParseDelStmtConfig
  { parseDelStmtExpr :: [Token] -> Either ParseError (ParseResult Expr),
    parseDelStmtPos :: Position,
    parseDelStmtTokenStream :: [Token]
  }
