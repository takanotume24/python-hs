module PythonHS.Parser.ParseWithStmtConfig (ParseWithStmtConfig (..)) where

import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)

-- | Configuration for parsing a with statement.
data ParseWithStmtConfig = ParseWithStmtConfig
  { parseWithStmtStatement :: [Token] -> Either ParseError (Stmt, [Token]),
    parseWithStmtPos :: Position
  }
