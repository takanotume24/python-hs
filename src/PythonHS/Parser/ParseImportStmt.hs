module PythonHS.Parser.ParseImportStmt (parseImportStmt) where

import PythonHS.AST.Stmt (Stmt (FromImportStmt, ImportStmt))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (AsToken, CommaToken, DotToken, EOFToken, FromToken, IdentifierToken, ImportToken, LParenToken, NewlineToken, RParenToken, StarToken))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))

parseImportStmt :: [Token] -> Either ParseError (Stmt, [Token])
parseImportStmt tokenStream =
  case tokenStream of
    Token ImportToken _ pos : rest -> do
      (entries, remaining) <- parseImportEntries rest
      Right (ImportStmt entries pos, remaining)
    Token FromToken _ pos : rest -> do
      (relativeLevel, modulePath, afterModulePath) <- parseFromModuleSpec rest
      case afterModulePath of
        Token ImportToken _ _ : afterImport -> do
          (items, remaining) <- parseFromItems afterImport
          Right (FromImportStmt relativeLevel modulePath items pos, remaining)
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

    parseFromItems ts =
      case ts of
        Token LParenToken _ _ : rest -> do
          (items, afterItems) <- parseFromItemsList rest
          case afterItems of
            Token RParenToken _ _ : remaining -> Right (items, remaining)
            tok : _ -> Left (ExpectedExpression (position tok))
            [] -> Left (ExpectedExpression (Position 0 0))
        _ -> parseFromItemsList ts

    parseFromItemsList ts = do
      (firstItem, afterFirst) <- parseFromItem ts
      parseFromItemsTail [firstItem] afterFirst

    parseFromItemsTail items ts =
      case ts of
        Token CommaToken _ _ : rest ->
          case rest of
            Token RParenToken _ _ : _ -> Right (items, rest)
            Token NewlineToken _ _ : _ -> Right (items, rest)
            Token EOFToken _ _ : _ -> Right (items, rest)
            _ -> do
              (nextItem, afterNext) <- parseFromItem rest
              parseFromItemsTail (items ++ [nextItem]) afterNext
        _ -> Right (items, ts)

    parseFromItem ts =
      case ts of
        Token StarToken _ _ : rest -> Right (("*", Nothing), rest)
        _ -> do
          (itemName, afterItem) <- parseIdentifier ts
          (aliasName, afterAlias) <- parseAlias afterItem
          Right ((itemName, aliasName), afterAlias)

    parseFromModuleSpec ts =
      case ts of
        Token DotToken _ _ : _ -> do
          (level, afterDots) <- parseLeadingDots 0 ts
          case afterDots of
            Token ImportToken _ _ : _ -> Right (level, [], afterDots)
            Token IdentifierToken _ _ : _ -> do
              (modulePath, remaining) <- parseModulePath afterDots
              Right (level, modulePath, remaining)
            tok : _ -> Left (ExpectedExpression (position tok))
            [] -> Left (ExpectedExpression (Position 0 0))
        _ -> do
          (modulePath, remaining) <- parseModulePath ts
          Right (0, modulePath, remaining)

    parseLeadingDots count ts =
      case ts of
        Token DotToken _ _ : rest -> parseLeadingDots (count + 1) rest
        _ -> Right (count, ts)

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
