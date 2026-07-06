module PythonHS.Parser.ParseDecoratedStmtConfig (ParseDecoratedStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult)

-- | Configuration for parsing a decorated statement.
data ParseDecoratedStmtConfig = ParseDecoratedStmtConfig
  { parseDecoratedStmtExpr :: [Token] -> Either ParseError (ParseResult Expr),
    parseDecoratedStmtStatement :: [Token] -> Either ParseError (ParseResult Stmt),
    parseDecoratedStmtPos :: Position,
    parseDecoratedStmtTokenStream :: [Token]
  }
