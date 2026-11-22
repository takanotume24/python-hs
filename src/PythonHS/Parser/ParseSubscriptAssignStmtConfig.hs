module PythonHS.Parser.ParseSubscriptAssignStmtConfig (ParseSubscriptAssignStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult)

data ParseSubscriptAssignStmtConfig = ParseSubscriptAssignStmtConfig
  { parseSubscriptAssignStmtExpr :: [Token] -> Either ParseError (ParseResult Expr),
    parseSubscriptAssignStmtTokenStream :: [Token]
  }
