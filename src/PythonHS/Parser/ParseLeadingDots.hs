module PythonHS.Parser.ParseLeadingDots (parseLeadingDots) where

import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (DotToken))
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseLeadingDotsConfig (ParseLeadingDotsConfig (..))
import PythonHS.Parser.ParseResult (ParseResult (..))

parseLeadingDots :: ParseLeadingDotsConfig -> Either ParseError (ParseResult Int)
parseLeadingDots config =
  case parseLeadingDotsTokenStream config of
    Token DotToken _ _ : rest -> parseLeadingDots (ParseLeadingDotsConfig {parseLeadingDotsCount = parseLeadingDotsCount config + 1, parseLeadingDotsTokenStream = rest})
    _ -> Right (ParseResult {parseResultValue = parseLeadingDotsCount config, parseResultRemaining = parseLeadingDotsTokenStream config})
