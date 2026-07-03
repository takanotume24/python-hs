module PythonHS.Parser.ParseIfTail (parseIfTail) where

import PythonHS.AST.Stmt (Stmt (IfStmt))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (ColonToken, ElifToken, ElseToken, NewlineToken))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))
import PythonHS.Parser.ParseExpr (parseExpr)
import PythonHS.Parser.ParseIfTailConfig (ParseIfTailConfig (..))

parseIfTail :: ParseIfTailConfig -> [Token] -> Either ParseError (Maybe [Stmt], [Token])
parseIfTail config ts =
  let parseSuiteFn = parseIfTailSuite config
   in case ts of
        Token ElseToken _ _ : Token ColonToken _ _ : afterElse -> do
          (elseSuite, finalRest) <- parseSuiteFn afterElse
          Right (Just elseSuite, finalRest)
        Token ElifToken _ elifPos : afterElif -> do
          (elifCond, afterElifCond) <- parseExpr afterElif
          case afterElifCond of
            Token ColonToken _ _ : afterElifColon -> do
              (elifThenSuite, afterElifThen) <- parseSuiteFn afterElifColon
              (elifElseBranch, finalRest) <- parseIfTail config afterElifThen
              Right (Just [IfStmt elifCond elifThenSuite elifElseBranch elifPos], finalRest)
            Token _ _ pos : _ -> Left (ExpectedExpression pos)
            _ -> Left (ExpectedExpression (Position 0 0))
        Token NewlineToken _ _ : _ ->
          case dropLeadingNewlines ts of
            Token ElseToken _ _ : Token ColonToken _ _ : afterElse -> do
              (elseSuite, finalRest) <- parseSuiteFn afterElse
              Right (Just elseSuite, finalRest)
            Token ElifToken _ elifPos : afterElif -> do
              (elifCond, afterElifCond) <- parseExpr afterElif
              case afterElifCond of
                Token ColonToken _ _ : afterElifColon -> do
                  (elifThenSuite, afterElifThen) <- parseSuiteFn afterElifColon
                  (elifElseBranch, finalRest) <- parseIfTail config afterElifThen
                  Right (Just [IfStmt elifCond elifThenSuite elifElseBranch elifPos], finalRest)
                Token _ _ pos : _ -> Left (ExpectedExpression pos)
                _ -> Left (ExpectedExpression (Position 0 0))
            _ -> Right (Nothing, ts)
        _ -> Right (Nothing, ts)
  where
    dropLeadingNewlines (Token NewlineToken _ _ : rest) = dropLeadingNewlines rest
    dropLeadingNewlines rest = rest
