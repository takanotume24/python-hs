module PythonHS.Parser.DropLeadingNewlines (dropLeadingNewlines) where

import PythonHS.Lexer.Token (Token(Token))
import PythonHS.Lexer.TokenType (TokenType(NewlineToken))

dropLeadingNewlines :: [Token] -> [Token]
dropLeadingNewlines (Token NewlineToken _ _ : rest) = dropLeadingNewlines rest
dropLeadingNewlines rest = rest
