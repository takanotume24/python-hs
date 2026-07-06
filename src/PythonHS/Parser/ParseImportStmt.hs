module PythonHS.Parser.ParseImportStmt (parseImportStmt) where

import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (FromToken, ImportToken))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseFromImport (parseFromImport)
import PythonHS.Parser.ParseFromImportResult (ParseFromImportResult (..))
import PythonHS.Parser.ParseFromItems (parseFromItems)
import PythonHS.Parser.ParseImportEntries (parseImportEntries)
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
