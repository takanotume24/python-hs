module PythonHS.Parser.ParseProgram (parseProgram) where

import PythonHS.AST.Program (Program (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType (TokenType (EOFToken, NewlineToken))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseResult (ParseResult (..))
import PythonHS.Parser.ParseStatement (parseStatement)

parseProgram :: [Token] -> Either ParseError Program
parseProgram tokens = fmap (Program . parseResultValue) (parseStatements tokens)
  where
    parseStatements [] = Right (ParseResult {parseResultValue = [], parseResultRemaining = []})
    parseStatements (Token EOFToken _ _ : rest) = Right (ParseResult {parseResultValue = [], parseResultRemaining = rest})
    parseStatements tokenStream = do
      stmtResult <- parseStatement tokenStream
      let statement = parseResultValue stmtResult
          restAfterStatement = parseResultRemaining stmtResult
      restAfterNewline <- consumeNewline restAfterStatement
      stmtsResult <- parseStatements restAfterNewline
      let otherStatements = parseResultValue stmtsResult
          finalRest = parseResultRemaining stmtsResult
      Right (ParseResult {parseResultValue = statement : otherStatements, parseResultRemaining = finalRest})

    consumeNewline (Token NewlineToken _ _ : rest) = Right rest
    consumeNewline (Token EOFToken _ pos : rest) = Right (Token {tokenType = EOFToken, lexeme = "", position = pos} : rest)
    consumeNewline (tok : _) = Left (ExpectedNewlineAfterStatement {parseErrorPosition = position tok})
    consumeNewline [] = Left (ExpectedNewlineAfterStatement {parseErrorPosition = Position {line = 0, column = 0}})
