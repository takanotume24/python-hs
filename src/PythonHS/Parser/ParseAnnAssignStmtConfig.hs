module PythonHS.Parser.ParseAnnAssignStmtConfig (ParseAnnAssignStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult)

data ParseAnnAssignStmtConfig = ParseAnnAssignStmtConfig
  { parseAnnAssignStmtExpr :: [Token] -> Either ParseError (ParseResult Expr),
    parseAnnAssignStmtName :: String,
    parseAnnAssignStmtPos :: Position,
    parseAnnAssignStmtTokenStream :: [Token]
  }
