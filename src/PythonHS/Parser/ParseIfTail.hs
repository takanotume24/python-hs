module PythonHS.Parser.ParseIfTail (parseIfTail) where

import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (ColonToken, ElifToken, ElseToken, NewlineToken))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseExpr (parseExpr)
import PythonHS.Parser.ParseIfTailConfig (ParseIfTailConfig (..))

parseIfTail :: ParseIfTailConfig -> Either ParseError (Maybe [Stmt], [Token])
parseIfTail config =
  let parseSuiteFn = parseIfTailSuite config
      ts = parseIfTailTokenStream config
   in case ts of
        Token ElseToken _ _ : Token ColonToken _ _ : afterElse -> do
          (elseSuite, finalRest) <- parseSuiteFn afterElse
          Right (Just elseSuite, finalRest)
        Token ElifToken _ elifPos : afterElif -> do
          (elifCond, afterElifCond) <- parseExpr afterElif
          case afterElifCond of
            Token ColonToken _ _ : afterElifColon -> do
              (elifThenSuite, afterElifThen) <- parseSuiteFn afterElifColon
              (elifElseBranch, finalRest) <- parseIfTail (config {parseIfTailTokenStream = afterElifThen})
              Right (Just [IfStmt {ifStmtCond = elifCond, ifStmtThen = elifThenSuite, ifStmtElse = elifElseBranch, ifStmtPos = elifPos}], finalRest)
            Token _ _ pos : _ -> Left (ExpectedExpression {parseErrorPosition = pos})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
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
                  (elifElseBranch, finalRest) <- parseIfTail (config {parseIfTailTokenStream = afterElifThen})
                  Right (Just [IfStmt {ifStmtCond = elifCond, ifStmtThen = elifThenSuite, ifStmtElse = elifElseBranch, ifStmtPos = elifPos}], finalRest)
                Token _ _ pos : _ -> Left (ExpectedExpression {parseErrorPosition = pos})
                _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
            _ -> Right (Nothing, ts)
        _ -> Right (Nothing, ts)
  where
    dropLeadingNewlines (Token NewlineToken _ _ : rest) = dropLeadingNewlines rest
    dropLeadingNewlines rest = rest
