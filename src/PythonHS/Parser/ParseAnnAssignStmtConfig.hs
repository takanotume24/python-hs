module PythonHS.Parser.ParseAnnAssignStmtConfig (ParseAnnAssignStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)

data ParseAnnAssignStmtConfig = ParseAnnAssignStmtConfig
  { parseAnnAssignStmtExpr :: [Token] -> Either ParseError (Expr, [Token]),
    parseAnnAssignStmtName :: String,
    parseAnnAssignStmtPos :: Position,
    parseAnnAssignStmtTokenStream :: [Token]
  }
