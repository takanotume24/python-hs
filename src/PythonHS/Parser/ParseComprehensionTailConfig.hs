module PythonHS.Parser.ParseComprehensionTailConfig (ParseComprehensionTailConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)

data ParseComprehensionTailConfig = ParseComprehensionTailConfig
  { parseComprehensionTailExpr :: [Token] -> Either ParseError (Expr, [Token]),
    parseComprehensionTailValueExpr :: Expr,
    parseComprehensionTailListPos :: Position,
    parseComprehensionTailClauses :: [([String], Expr, [Expr])],
    parseComprehensionTailTokenStream :: [Token]
  }
