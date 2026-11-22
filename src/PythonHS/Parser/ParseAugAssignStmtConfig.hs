module PythonHS.Parser.ParseAugAssignStmtConfig (ParseAugAssignStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult)

data ParseAugAssignStmtConfig = ParseAugAssignStmtConfig
  { parseAugAssignStmtExpr :: [Token] -> Either ParseError (ParseResult Expr),
    parseAugAssignStmtTokenStream :: [Token]
  }
