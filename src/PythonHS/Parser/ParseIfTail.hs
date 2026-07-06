module PythonHS.Parser.ParseIfTail (parseIfTail) where

import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (ColonToken, ElifToken, ElseToken, NewlineToken))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseExpr (parseExpr)
import PythonHS.Parser.ParseIfTailConfig (ParseIfTailConfig (..))
import PythonHS.Parser.ParseResult (ParseResult (..))

parseIfTail :: ParseIfTailConfig -> Either ParseError (ParseResult (Maybe [Stmt]))
parseIfTail config =
  let parseSuiteFn = parseIfTailSuite config
      ts = parseIfTailTokenStream config
   in case ts of
        Token ElseToken _ _ : Token ColonToken _ _ : afterElse -> do
          elseResult <- parseSuiteFn afterElse
          let elseSuite = parseResultValue elseResult
              finalRest = parseResultRemaining elseResult
          Right (ParseResult {parseResultValue = Just elseSuite, parseResultRemaining = finalRest})
        Token ElifToken _ elifPos : afterElif -> do
          condResult <- parseExpr afterElif
          let elifCond = parseResultValue condResult
              afterElifCond = parseResultRemaining condResult
          case afterElifCond of
            Token ColonToken _ _ : afterElifColon -> do
              thenResult <- parseSuiteFn afterElifColon
              let elifThenSuite = parseResultValue thenResult
                  afterElifThen = parseResultRemaining thenResult
              elseResult <- parseIfTail (config {parseIfTailTokenStream = afterElifThen})
              let elifElseBranch = parseResultValue elseResult
                  finalRest = parseResultRemaining elseResult
              Right (ParseResult {parseResultValue = Just [IfStmt {ifStmtCond = elifCond, ifStmtThen = elifThenSuite, ifStmtElse = elifElseBranch, ifStmtPos = elifPos}], parseResultRemaining = finalRest})
            Token _ _ pos : _ -> Left (ExpectedExpression {parseErrorPosition = pos})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
        Token NewlineToken _ _ : _ ->
          case dropLeadingNewlines ts of
            Token ElseToken _ _ : Token ColonToken _ _ : afterElse -> do
              elseResult <- parseSuiteFn afterElse
              let elseSuite = parseResultValue elseResult
                  finalRest = parseResultRemaining elseResult
              Right (ParseResult {parseResultValue = Just elseSuite, parseResultRemaining = finalRest})
            Token ElifToken _ elifPos : afterElif -> do
              condResult <- parseExpr afterElif
              let elifCond = parseResultValue condResult
                  afterElifCond = parseResultRemaining condResult
              case afterElifCond of
                Token ColonToken _ _ : afterElifColon -> do
                  thenResult <- parseSuiteFn afterElifColon
                  let elifThenSuite = parseResultValue thenResult
                      afterElifThen = parseResultRemaining thenResult
                  elseResult <- parseIfTail (config {parseIfTailTokenStream = afterElifThen})
                  let elifElseBranch = parseResultValue elseResult
                      finalRest = parseResultRemaining elseResult
                  Right (ParseResult {parseResultValue = Just [IfStmt {ifStmtCond = elifCond, ifStmtThen = elifThenSuite, ifStmtElse = elifElseBranch, ifStmtPos = elifPos}], parseResultRemaining = finalRest})
                Token _ _ pos : _ -> Left (ExpectedExpression {parseErrorPosition = pos})
                _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
            _ -> Right (ParseResult {parseResultValue = Nothing, parseResultRemaining = ts})
        _ -> Right (ParseResult {parseResultValue = Nothing, parseResultRemaining = ts})
  where
    dropLeadingNewlines (Token NewlineToken _ _ : rest) = dropLeadingNewlines rest
    dropLeadingNewlines rest = rest
