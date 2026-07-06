module PythonHS.Parser.ParseModuleTail (parseModuleTail) where

import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (DotToken))
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseIdentifier (parseIdentifier)
import PythonHS.Parser.ParseModuleTailConfig (ParseModuleTailConfig (..))
import PythonHS.Parser.ParseResult (ParseResult (..))

parseModuleTail :: ParseModuleTailConfig -> Either ParseError (ParseResult [String])
parseModuleTail config =
  case parseModuleTailTokenStream config of
    Token DotToken _ _ : rest -> do
      nextNameResult <- parseIdentifier rest
      let nextName = parseResultValue nextNameResult
          afterName = parseResultRemaining nextNameResult
      parseModuleTail (ParseModuleTailConfig {parseModuleTailSegments = parseModuleTailSegments config ++ [nextName], parseModuleTailTokenStream = afterName})
    _ -> Right (ParseResult {parseResultValue = parseModuleTailSegments config, parseResultRemaining = parseModuleTailTokenStream config})
