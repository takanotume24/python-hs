module PythonHS.Parser.ParseSuite (parseSuite) where

import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType (TokenType (DedentToken, IndentToken, NewlineToken))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseSuiteConfig (ParseSuiteConfig (..))

parseSuite ::
  ParseSuiteConfig ->
  Either ParseError ([Stmt], [Token])
parseSuite config =
  let ts = parseSuiteTokenStream config
   in case ts of
        Token {tokenType = NewlineToken} : Token {tokenType = IndentToken} : rest -> parseIndentedSuite rest
        Token {tokenType = NewlineToken} : rest -> do
          (stmt, remaining) <- parseStatement rest
          Right ([stmt], remaining)
        _ -> do
          (stmt, remaining) <- parseStatement ts
          Right ([stmt], remaining)
  where
    parseStatement = parseSuiteStatement config
    parseIndentedSuite (Token {tokenType = DedentToken, position = dedentPos} : rest) =
      Right ([], Token {tokenType = NewlineToken, lexeme = "\\n", position = dedentPos} : rest)
    parseIndentedSuite input = do
      (statement, restAfterStatement) <- parseStatement input
      restAfterNewline <- consumeNewline restAfterStatement
      case restAfterNewline of
        Token {tokenType = DedentToken, position = dedentPos} : rest ->
          Right ([statement], Token {tokenType = NewlineToken, lexeme = "\\n", position = dedentPos} : rest)
        _ -> do
          (otherStatements, finalRest) <- parseIndentedSuite restAfterNewline
          Right (statement : otherStatements, finalRest)

    consumeNewline (Token {tokenType = NewlineToken} : rest) = Right rest
    consumeNewline rest@((Token {tokenType = DedentToken}) : _) = Right rest
    consumeNewline (Token {position = pos} : _) = Left (ExpectedNewlineAfterStatement {parseErrorPosition = pos})
    consumeNewline [] = Left (ExpectedNewlineAfterStatement {parseErrorPosition = Position {line = 0, column = 0}})
