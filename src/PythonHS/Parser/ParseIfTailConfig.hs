module PythonHS.Parser.ParseIfTailConfig (ParseIfTailConfig (..)) where

import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult)

data ParseIfTailConfig = ParseIfTailConfig
  { parseIfTailSuite :: [Token] -> Either ParseError (ParseResult [Stmt]),
    parseIfTailTokenStream :: [Token]
  }
