module PythonHS.Parser.ParseImportStmt (parseImportStmt) where

import PythonHS.AST.FromImportEntry (FromImportEntry (..))
import PythonHS.AST.ImportEntry (ImportEntry (..))
import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (AsToken, CommaToken, DotToken, EOFToken, FromToken, IdentifierToken, ImportToken, LParenToken, NewlineToken, RParenToken, StarToken))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseResult (ParseResult (..))

parseImportStmt :: [Token] -> Either ParseError (ParseResult Stmt)
parseImportStmt tokenStream =
  case tokenStream of
    Token ImportToken _ pos : rest -> do
      entriesResult <- parseImportEntries rest
      let entries = parseResultValue entriesResult
          remaining = parseResultRemaining entriesResult
      Right (ParseResult {parseResultValue = ImportStmt {importStmtItems = entries, importStmtPos = pos}, parseResultRemaining = remaining})
    Token FromToken _ pos : rest -> do
      fromResult <- parseFromImport rest
      let fromLevel = parseFromImportResultLevel fromResult
          modulePath = parseFromImportResultModulePath fromResult
          afterModulePath = parseFromImportResultRemaining fromResult
      case afterModulePath of
        Token ImportToken _ _ : afterImport -> do
          itemsResult <- parseFromItems afterImport
          let items = parseResultValue itemsResult
              remaining = parseResultRemaining itemsResult
          Right (ParseResult {parseResultValue = FromImportStmt {fromImportStmtLevel = fromLevel, fromImportStmtModule = modulePath, fromImportStmtItems = items, fromImportStmtPos = pos}, parseResultRemaining = remaining})
        tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
        [] -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
    tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
    [] -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})

-- Internal record for from-import parsing to avoid tuples
data ParseFromImportResult = ParseFromImportResult
  { parseFromImportResultLevel :: Int,
    parseFromImportResultModulePath :: [String],
    parseFromImportResultRemaining :: [Token]
  }

parseFromImport :: [Token] -> Either ParseError ParseFromImportResult
parseFromImport ts =
  case ts of
    Token DotToken _ _ : _ -> do
      dotsResult <- parseLeadingDots 0 ts
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

parseFromItemsList :: [Token] -> Either ParseError (ParseResult [FromImportEntry])
parseFromItemsList ts = do
  firstItemResult <- parseFromItem ts
  let firstItem = parseResultValue firstItemResult
      afterFirst = parseResultRemaining firstItemResult
  parseFromItemsTail [firstItem] afterFirst

parseFromItemsTail :: [FromImportEntry] -> [Token] -> Either ParseError (ParseResult [FromImportEntry])
parseFromItemsTail items ts =
  case ts of
    Token CommaToken _ _ : rest ->
      case rest of
        Token RParenToken _ _ : _ -> Right (ParseResult {parseResultValue = items, parseResultRemaining = rest})
        Token NewlineToken _ _ : _ -> Right (ParseResult {parseResultValue = items, parseResultRemaining = rest})
        Token EOFToken _ _ : _ -> Right (ParseResult {parseResultValue = items, parseResultRemaining = rest})
        _ -> do
          nextItemResult <- parseFromItem rest
          let nextItem = parseResultValue nextItemResult
              afterNext = parseResultRemaining nextItemResult
          parseFromItemsTail (items ++ [nextItem]) afterNext
    _ -> Right (ParseResult {parseResultValue = items, parseResultRemaining = ts})

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

parseLeadingDots :: Int -> [Token] -> Either ParseError (ParseResult Int)
parseLeadingDots count ts =
  case ts of
    Token DotToken _ _ : rest -> parseLeadingDots (count + 1) rest
    _ -> Right (ParseResult {parseResultValue = count, parseResultRemaining = ts})

parseModulePath :: [Token] -> Either ParseError (ParseResult [String])
parseModulePath ts = do
  firstNameResult <- parseIdentifier ts
  let firstName = parseResultValue firstNameResult
      afterFirst = parseResultRemaining firstNameResult
  parseModuleTail [firstName] afterFirst

parseModuleTail :: [String] -> [Token] -> Either ParseError (ParseResult [String])
parseModuleTail segments ts =
  case ts of
    Token DotToken _ _ : rest -> do
      nextNameResult <- parseIdentifier rest
      let nextName = parseResultValue nextNameResult
          afterName = parseResultRemaining nextNameResult
      parseModuleTail (segments ++ [nextName]) afterName
    _ -> Right (ParseResult {parseResultValue = segments, parseResultRemaining = ts})

parseAlias :: [Token] -> Either ParseError (ParseResult (Maybe String))
parseAlias ts =
  case ts of
    Token AsToken _ _ : rest -> do
      aliasNameResult <- parseIdentifier rest
      let aliasName = parseResultValue aliasNameResult
          remaining = parseResultRemaining aliasNameResult
      Right (ParseResult {parseResultValue = Just aliasName, parseResultRemaining = remaining})
    _ -> Right (ParseResult {parseResultValue = Nothing, parseResultRemaining = ts})

parseIdentifier :: [Token] -> Either ParseError (ParseResult String)
parseIdentifier ts =
  case ts of
    Token IdentifierToken name _ : rest -> Right (ParseResult {parseResultValue = name, parseResultRemaining = rest})
    tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
    [] -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
