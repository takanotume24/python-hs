module PythonHS.Parser.ParseClassStmtConfig (ParseClassStmtConfig (..)) where

import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult)

-- | Configuration for parsing a class statement.
data ParseClassStmtConfig = ParseClassStmtConfig
  { parseClassStmtSuite :: [Token] -> Either ParseError (ParseResult [Stmt]),
    parseClassStmtPos :: Position,
    parseClassStmtName :: String,
    parseClassStmtTokenStream :: [Token]
  }
