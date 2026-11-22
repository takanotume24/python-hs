module PythonHS.Parser.ParseClassStmt (parseClassStmt) where

import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType (TokenType (ColonToken, DotToken, IdentifierToken, LParenToken, RParenToken))
import PythonHS.Parser.ParseClassStmtConfig (ParseClassStmtConfig (..))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseResult (ParseResult (..))

parseClassStmt :: ParseClassStmtConfig -> Either ParseError (ParseResult Stmt)
parseClassStmt config =
  let parseSuiteFn = parseClassStmtSuite config
      posClass = parseClassStmtPos config
      className = parseClassStmtName config
      rest = parseClassStmtTokenStream config
   in case rest of
        Token {tokenType = ColonToken} : afterColon ->
          buildClassStmt parseSuiteFn posClass className Nothing afterColon
        Token {tokenType = LParenToken} : afterParen ->
          case consumeDottedName afterParen of
            Just (baseName, Token {tokenType = RParenToken} : Token {tokenType = ColonToken} : afterColon) ->
              buildClassStmt parseSuiteFn posClass className (Just baseName) afterColon
            _ -> Left (ExpectedExpression {parseErrorPosition = positionAfterParen afterParen})
        Token {position = pos'} : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
        _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
  where
    positionAfterParen (Token {position = pos'} : _) = pos'
    positionAfterParen [] = Position {line = 0, column = 0}

    consumeDottedName (Token {tokenType = IdentifierToken, lexeme = firstName} : afterFirst) =
      Just (collectDottedName firstName afterFirst)
    consumeDottedName _ = Nothing

    collectDottedName firstName (Token {tokenType = DotToken} : Token {tokenType = IdentifierToken, lexeme = nextName} : afterNext) =
      collectDottedName (firstName ++ "." ++ nextName) afterNext
    collectDottedName accumulatedName remainingTokens = (accumulatedName, remainingTokens)

    buildClassStmt parseSuiteFn posClass className maybeBase afterColon = do
      suiteResult <- parseSuiteFn afterColon
      let bodySuite = parseResultValue suiteResult
          finalRest = parseResultRemaining suiteResult
      Right (ParseResult {parseResultValue = ClassDefStmt {classDefStmtName = className, classDefStmtBase = maybeBase, classDefStmtBody = bodySuite, classDefStmtPos = posClass}, parseResultRemaining = finalRest})
