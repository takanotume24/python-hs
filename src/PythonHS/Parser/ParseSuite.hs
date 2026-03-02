module PythonHS.Parser.ParseSuite (parseSuite) where

import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (DedentToken, IndentToken, NewlineToken))
import PythonHS.Parser.ParseError (ParseError (ExpectedNewlineAfterStatement))

parseSuite ::
  ([Token] -> Either ParseError (Stmt, [Token])) ->
  [Token] ->
  Either ParseError ([Stmt], [Token])
parseSuite parseStatement ts =
  case ts of
    Token NewlineToken _ _ : Token IndentToken _ _ : rest -> parseIndentedSuite rest
    Token NewlineToken _ _ : rest -> do
      (stmt, remaining) <- parseStatement rest
      Right ([stmt], remaining)
    _ -> do
      (stmt, remaining) <- parseStatement ts
      Right ([stmt], remaining)
  where
    parseIndentedSuite (Token DedentToken _ dedentPos : rest) =
      Right ([], Token NewlineToken "\\n" dedentPos : rest)
    parseIndentedSuite input = do
      (statement, restAfterStatement) <- parseStatement input
      restAfterNewline <- consumeNewline restAfterStatement
      case restAfterNewline of
        Token DedentToken _ dedentPos : rest ->
          Right ([statement], Token NewlineToken "\\n" dedentPos : rest)
        _ -> do
          (otherStatements, finalRest) <- parseIndentedSuite restAfterNewline
          Right (statement : otherStatements, finalRest)

    consumeNewline (Token NewlineToken _ _ : rest) = Right rest
    consumeNewline (Token _ _ pos : _) = Left (ExpectedNewlineAfterStatement pos)
    consumeNewline [] = Left (ExpectedNewlineAfterStatement (Position 0 0))
