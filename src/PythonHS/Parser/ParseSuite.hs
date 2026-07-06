module PythonHS.Parser.ParseSuite (parseSuite) where

import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType (TokenType (DedentToken, IndentToken, NewlineToken))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseResult (ParseResult (..))
import PythonHS.Parser.ParseSuiteConfig (ParseSuiteConfig (..))

parseSuite ::
  ParseSuiteConfig ->
  Either ParseError (ParseResult [Stmt])
parseSuite config =
  let ts = parseSuiteTokenStream config
   in case ts of
        Token {tokenType = NewlineToken} : Token {tokenType = IndentToken} : rest -> parseIndentedSuite rest
        Token {tokenType = NewlineToken} : rest -> do
          stmtResult <- parseStatement rest
          let stmt = parseResultValue stmtResult
              remaining = parseResultRemaining stmtResult
          Right (ParseResult {parseResultValue = [stmt], parseResultRemaining = remaining})
        _ -> do
          stmtResult <- parseStatement ts
          let stmt = parseResultValue stmtResult
              remaining = parseResultRemaining stmtResult
          Right (ParseResult {parseResultValue = [stmt], parseResultRemaining = remaining})
  where
    parseStatement = parseSuiteStatement config
    parseIndentedSuite (Token {tokenType = DedentToken, position = dedentPos} : rest) =
      Right (ParseResult {parseResultValue = [], parseResultRemaining = Token {tokenType = NewlineToken, lexeme = "\\n", position = dedentPos} : rest})
    parseIndentedSuite input = do
      statementResult <- parseStatement input
      let statement = parseResultValue statementResult
          restAfterStatement = parseResultRemaining statementResult
      restAfterNewline <- consumeNewline restAfterStatement
      case restAfterNewline of
        Token {tokenType = DedentToken, position = dedentPos} : rest ->
          Right (ParseResult {parseResultValue = [statement], parseResultRemaining = Token {tokenType = NewlineToken, lexeme = "\\n", position = dedentPos} : rest})
        _ -> do
          otherResult <- parseIndentedSuite restAfterNewline
          let otherStatements = parseResultValue otherResult
              finalRest = parseResultRemaining otherResult
          Right (ParseResult {parseResultValue = statement : otherStatements, parseResultRemaining = finalRest})

    consumeNewline (Token {tokenType = NewlineToken} : rest) = Right rest
    consumeNewline rest@((Token {tokenType = DedentToken}) : _) = Right rest
    consumeNewline (Token {position = pos} : _) = Left (ExpectedNewlineAfterStatement {parseErrorPosition = pos})
    consumeNewline [] = Left (ExpectedNewlineAfterStatement {parseErrorPosition = Position {line = 0, column = 0}})
