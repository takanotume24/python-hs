module PythonHS.Parser.ParseFromImport (parseFromImport) where

import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (DotToken, IdentifierToken, ImportToken))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseFromImportResult (ParseFromImportResult (..))
import PythonHS.Parser.ParseLeadingDots (parseLeadingDots)
import PythonHS.Parser.ParseLeadingDotsConfig (ParseLeadingDotsConfig (..))
import PythonHS.Parser.ParseModulePath (parseModulePath)
import PythonHS.Parser.ParseResult (ParseResult (..))

parseFromImport :: [Token] -> Either ParseError ParseFromImportResult
parseFromImport ts =
  case ts of
    Token DotToken _ _ : _ -> do
      dotsResult <- parseLeadingDots (ParseLeadingDotsConfig {parseLeadingDotsCount = 0, parseLeadingDotsTokenStream = ts})
      let level = parseResultValue dotsResult
          afterDots = parseResultRemaining dotsResult
      case afterDots of
        Token ImportToken _ _ : _ -> Right (ParseFromImportResult {parseFromImportResultLevel = level, parseFromImportResultModulePath = [], parseFromImportResultRemaining = afterDots})
        Token IdentifierToken _ _ : _ -> do
          pathResult <- parseModulePath afterDots
          let modulePath = parseResultValue pathResult
              remaining = parseResultRemaining pathResult
          Right (ParseFromImportResult {parseFromImportResultLevel = level, parseFromImportResultModulePath = modulePath, parseFromImportResultRemaining = remaining})
        tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
        [] -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
    _ -> do
      pathResult <- parseModulePath ts
      let modulePath = parseResultValue pathResult
          remaining = parseResultRemaining pathResult
      Right (ParseFromImportResult {parseFromImportResultLevel = 0, parseFromImportResultModulePath = modulePath, parseFromImportResultRemaining = remaining})
