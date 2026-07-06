module PythonHS.Parser.ParseFromItemsTail (parseFromItemsTail) where

import PythonHS.AST.FromImportEntry (FromImportEntry (..))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (CommaToken, EOFToken, NewlineToken, RParenToken))
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseFromItem (parseFromItem)
import PythonHS.Parser.ParseFromItemsTailConfig (ParseFromItemsTailConfig (..))
import PythonHS.Parser.ParseResult (ParseResult (..))

parseFromItemsTail :: ParseFromItemsTailConfig -> Either ParseError (ParseResult [FromImportEntry])
parseFromItemsTail config =
  case parseFromItemsTailTokenStream config of
    Token CommaToken _ _ : rest ->
      case rest of
        Token RParenToken _ _ : _ -> Right (ParseResult {parseResultValue = parseFromItemsTailItems config, parseResultRemaining = rest})
        Token NewlineToken _ _ : _ -> Right (ParseResult {parseResultValue = parseFromItemsTailItems config, parseResultRemaining = rest})
        Token EOFToken _ _ : _ -> Right (ParseResult {parseResultValue = parseFromItemsTailItems config, parseResultRemaining = rest})
        _ -> do
          nextItemResult <- parseFromItem rest
          let nextItem = parseResultValue nextItemResult
              afterNext = parseResultRemaining nextItemResult
          parseFromItemsTail (ParseFromItemsTailConfig {parseFromItemsTailItems = parseFromItemsTailItems config ++ [nextItem], parseFromItemsTailTokenStream = afterNext})
    _ -> Right (ParseResult {parseResultValue = parseFromItemsTailItems config, parseResultRemaining = parseFromItemsTailTokenStream config})
