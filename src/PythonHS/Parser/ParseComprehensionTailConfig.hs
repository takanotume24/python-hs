module PythonHS.Parser.ParseComprehensionTailConfig (ParseComprehensionTailConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.ListComprehensionClause (ListComprehensionClause)
import PythonHS.Lexer.Position (Position)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult)

data ParseComprehensionTailConfig = ParseComprehensionTailConfig
  { parseComprehensionTailExpr :: [Token] -> Either ParseError (ParseResult Expr),
    parseComprehensionTailValueExpr :: Expr,
    parseComprehensionTailListPos :: Position,
    parseComprehensionTailClauses :: [ListComprehensionClause Expr],
    parseComprehensionTailTokenStream :: [Token]
  }
