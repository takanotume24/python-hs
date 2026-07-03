module PythonHS.Parser.ParseIfTailConfig (ParseIfTailConfig (..)) where

import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)

data ParseIfTailConfig = ParseIfTailConfig
  { parseIfTailSuite :: [Token] -> Either ParseError ([Stmt], [Token])
  }
