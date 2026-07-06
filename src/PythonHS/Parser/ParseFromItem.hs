module PythonHS.Parser.ParseFromItem (parseFromItem) where

import PythonHS.AST.FromImportEntry (FromImportEntry (..))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (StarToken))
import PythonHS.Parser.ParseAlias (parseAlias)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseIdentifier (parseIdentifier)
import PythonHS.Parser.ParseResult (ParseResult (..))

parseFromItem :: [Token] -> Either ParseError (ParseResult FromImportEntry)
parseFromItem ts =
  case ts of
    Token StarToken _ _ : rest -> Right (ParseResult {parseResultValue = FromImportEntry {fromImportEntryName = "*", fromImportEntryAlias = Nothing}, parseResultRemaining = rest})
    _ -> do
      itemResult <- parseIdentifier ts
      let itemName = parseResultValue itemResult
          afterItem = parseResultRemaining itemResult
      aliasResult <- parseAlias afterItem
      let aliasName = parseResultValue aliasResult
          afterAlias = parseResultRemaining aliasResult
      Right (ParseResult {parseResultValue = FromImportEntry {fromImportEntryName = itemName, fromImportEntryAlias = aliasName}, parseResultRemaining = afterAlias})
