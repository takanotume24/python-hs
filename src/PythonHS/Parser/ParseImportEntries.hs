module PythonHS.Parser.ParseImportEntries (parseImportEntries) where

import PythonHS.AST.ImportEntry (ImportEntry (..))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (CommaToken))
import PythonHS.Parser.ParseAlias (parseAlias)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseModulePath (parseModulePath)
import PythonHS.Parser.ParseResult (ParseResult (..))

parseImportEntries :: [Token] -> Either ParseError (ParseResult [ImportEntry])
parseImportEntries ts = do
  pathResult <- parseModulePath ts
  let modulePath = parseResultValue pathResult
      afterPath = parseResultRemaining pathResult
  aliasResult <- parseAlias afterPath
  let aliasName = parseResultValue aliasResult
      afterAlias = parseResultRemaining aliasResult
  case afterAlias of
    Token CommaToken _ _ : rest -> do
      entriesResult <- parseImportEntries rest
      let otherEntries = parseResultValue entriesResult
          remaining = parseResultRemaining entriesResult
      Right (ParseResult {parseResultValue = ImportEntry {importEntryPath = modulePath, importEntryAlias = aliasName} : otherEntries, parseResultRemaining = remaining})
    _ -> Right (ParseResult {parseResultValue = [ImportEntry {importEntryPath = modulePath, importEntryAlias = aliasName}], parseResultRemaining = afterAlias})
