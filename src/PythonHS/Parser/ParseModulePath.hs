module PythonHS.Parser.ParseModulePath (parseModulePath) where

import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseIdentifier (parseIdentifier)
import PythonHS.Parser.ParseModuleTail (parseModuleTail)
import PythonHS.Parser.ParseModuleTailConfig (ParseModuleTailConfig (..))
import PythonHS.Parser.ParseResult (ParseResult (..))

parseModulePath :: [Token] -> Either ParseError (ParseResult [String])
parseModulePath ts = do
  firstNameResult <- parseIdentifier ts
  let firstName = parseResultValue firstNameResult
      afterFirst = parseResultRemaining firstNameResult
  parseModuleTail (ParseModuleTailConfig {parseModuleTailSegments = [firstName], parseModuleTailTokenStream = afterFirst})
