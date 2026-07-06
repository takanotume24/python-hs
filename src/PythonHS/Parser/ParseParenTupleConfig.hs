module PythonHS.Parser.ParseParenTupleConfig (ParseParenTupleConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult)

data ParseParenTupleConfig = ParseParenTupleConfig
  { parseParenTupleExpr :: [Token] -> Either ParseError (ParseResult Expr),
    parseParenTuplePos :: Position,
    parseParenTupleTokenStream :: [Token]
  }
