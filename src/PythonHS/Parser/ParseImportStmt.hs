module PythonHS.Parser.ParseImportStmt (parseImportStmt) where

import PythonHS.AST.Stmt (Stmt (FromImportStmt, ImportStmt))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (AsToken, CommaToken, DotToken, FromToken, IdentifierToken, ImportToken))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))

parseImportStmt :: [Token] -> Either ParseError (Stmt, [Token])
parseImportStmt tokenStream =
  case tokenStream of
    Token ImportToken _ pos : rest -> do
      (entries, remaining) <- parseImportEntries rest
      Right (ImportStmt entries pos, remaining)
    Token FromToken _ pos : rest -> do
      (modulePath, afterModulePath) <- parseModulePath rest
      case afterModulePath of
        Token ImportToken _ _ : afterImport -> do
          (items, remaining) <- parseFromItems afterImport
          Right (FromImportStmt modulePath items pos, remaining)
        tok : _ -> Left (ExpectedExpression (position tok))
        [] -> Left (ExpectedExpression (Position 0 0))
    tok : _ -> Left (ExpectedExpression (position tok))
    [] -> Left (ExpectedExpression (Position 0 0))
  where
    parseImportEntries ts = do
      (modulePath, afterPath) <- parseModulePath ts
      (aliasName, afterAlias) <- parseAlias afterPath
      case afterAlias of
        Token CommaToken _ _ : rest -> do
          (otherEntries, remaining) <- parseImportEntries rest
          Right ((modulePath, aliasName) : otherEntries, remaining)
        _ -> Right ([(modulePath, aliasName)], afterAlias)

    parseFromItems ts = do
      (itemName, afterItem) <- parseIdentifier ts
      (aliasName, afterAlias) <- parseAlias afterItem
      case afterAlias of
        Token CommaToken _ _ : rest -> do
          (otherItems, remaining) <- parseFromItems rest
          Right ((itemName, aliasName) : otherItems, remaining)
        _ -> Right ([(itemName, aliasName)], afterAlias)

    parseModulePath ts = do
      (firstName, afterFirst) <- parseIdentifier ts
      parseModuleTail [firstName] afterFirst

    parseModuleTail segments ts =
      case ts of
        Token DotToken _ _ : rest -> do
          (nextName, afterName) <- parseIdentifier rest
          parseModuleTail (segments ++ [nextName]) afterName
        _ -> Right (segments, ts)

    parseAlias ts =
      case ts of
        Token AsToken _ _ : rest -> do
          (aliasName, remaining) <- parseIdentifier rest
          Right (Just aliasName, remaining)
        _ -> Right (Nothing, ts)

    parseIdentifier ts =
      case ts of
        Token IdentifierToken name _ : rest -> Right (name, rest)
        tok : _ -> Left (ExpectedExpression (position tok))
        [] -> Left (ExpectedExpression (Position 0 0))
