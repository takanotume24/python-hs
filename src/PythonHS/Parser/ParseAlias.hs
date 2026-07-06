module PythonHS.Parser.ParseAlias (parseAlias) where

import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (AsToken))
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseIdentifier (parseIdentifier)
import PythonHS.Parser.ParseResult (ParseResult (..))

parseAlias :: [Token] -> Either ParseError (ParseResult (Maybe String))
parseAlias ts =
  case ts of
    Token AsToken _ _ : rest -> do
      aliasNameResult <- parseIdentifier rest
      let aliasName = parseResultValue aliasNameResult
          remaining = parseResultRemaining aliasNameResult
      Right (ParseResult {parseResultValue = Just aliasName, parseResultRemaining = remaining})
    _ -> Right (ParseResult {parseResultValue = Nothing, parseResultRemaining = ts})
