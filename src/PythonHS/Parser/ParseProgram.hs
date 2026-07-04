module PythonHS.Parser.ParseProgram (parseProgram) where

import PythonHS.AST.Program (Program (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType (TokenType (EOFToken, NewlineToken))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseStatement (parseStatement)

parseProgram :: [Token] -> Either ParseError Program
parseProgram tokens = fmap (Program . fst) (parseStatements tokens)
  where
    parseStatements [] = Right ([], [])
    parseStatements (Token EOFToken _ _ : rest) = Right ([], rest)
    parseStatements tokenStream = do
      (statement, restAfterStatement) <- parseStatement tokenStream
      restAfterNewline <- consumeNewline restAfterStatement
      (otherStatements, finalRest) <- parseStatements restAfterNewline
      Right (statement : otherStatements, finalRest)

    consumeNewline (Token NewlineToken _ _ : rest) = Right rest
    consumeNewline (Token EOFToken _ pos : rest) = Right (Token {tokenType = EOFToken, lexeme = "", position = pos} : rest)
    consumeNewline (tok : _) = Left (ExpectedNewlineAfterStatement {parseErrorPosition = position tok})
    consumeNewline [] = Left (ExpectedNewlineAfterStatement {parseErrorPosition = Position {line = 0, column = 0}})
