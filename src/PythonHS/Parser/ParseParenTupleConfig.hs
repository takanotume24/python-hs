module PythonHS.Parser.ParseParenTupleConfig (ParseParenTupleConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)

data ParseParenTupleConfig = ParseParenTupleConfig
  { parseParenTupleExpr :: [Token] -> Either ParseError (Expr, [Token]),
    parseParenTuplePos :: Position
  }
