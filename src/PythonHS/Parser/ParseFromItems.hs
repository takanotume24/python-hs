module PythonHS.Parser.ParseFromItems (parseFromItems) where

import PythonHS.AST.FromImportEntry (FromImportEntry (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (LParenToken, RParenToken))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseFromItemsList (parseFromItemsList)
import PythonHS.Parser.ParseResult (ParseResult (..))

parseFromItems :: [Token] -> Either ParseError (ParseResult [FromImportEntry])
parseFromItems ts =
  case ts of
    Token LParenToken _ _ : rest -> do
      itemsResult <- parseFromItemsList rest
      let items = parseResultValue itemsResult
          afterItems = parseResultRemaining itemsResult
      case afterItems of
        Token RParenToken _ _ : remaining -> Right (ParseResult {parseResultValue = items, parseResultRemaining = remaining})
        tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
        [] -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
    _ -> parseFromItemsList ts
