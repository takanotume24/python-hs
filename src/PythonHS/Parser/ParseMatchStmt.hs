module PythonHS.Parser.ParseMatchStmt (parseMatchStmt) where

import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType (TokenType (CaseToken, ColonToken, DedentToken, IfToken, IndentToken, NewlineToken))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseMatchStmtConfig (ParseMatchStmtConfig (..))
import PythonHS.Parser.ParsePattern (parsePattern)
import PythonHS.Parser.ParsePatternConfig (ParsePatternConfig (..))

parseMatchStmt ::
  ParseMatchStmtConfig ->
  Position ->
  [Token] ->
  Either ParseError (Stmt, [Token])
parseMatchStmt config pos rest = do
  let parseExprFn = parseMatchStmtExpr config
      parseSuiteFn = parseMatchStmtSuite config
  (subjectExpr, afterSubject) <- parseExprFn rest
  case afterSubject of
    Token ColonToken _ _ : Token NewlineToken _ _ : Token IndentToken _ _ : afterIndent -> do
      (cases, afterCases) <- parseCaseClauses parseExprFn parseSuiteFn afterIndent
      Right (MatchStmt {matchStmtSubject = subjectExpr, matchStmtCases = cases, matchStmtPos = pos}, afterCases)
    Token ColonToken _ _ : _ -> Left (ExpectedExpression {parseErrorPosition = pos})
    tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
    _ -> Left (ExpectedExpression {parseErrorPosition = Position 0 0})
  where
    parseCaseClauses _ _ (Token DedentToken _ dedentPos : _) =
      Left (ExpectedExpression {parseErrorPosition = dedentPos})
    parseCaseClauses parseExprFn parseSuiteFn ts = do
      (firstCase, afterFirst) <- parseCaseClause parseExprFn parseSuiteFn ts
      parseCaseTail parseExprFn parseSuiteFn [firstCase] afterFirst

    parseCaseTail _ _ acc (Token DedentToken _ dedentPos : restTokens) =
      Right (reverse acc, Token {tokenType = NewlineToken, lexeme = "\\n", position = dedentPos} : restTokens)
    parseCaseTail parseExprFn parseSuiteFn acc ts = do
      restAfterNewline <- consumeNewline ts
      case restAfterNewline of
        Token DedentToken _ dedentPos : restTokens ->
          Right (reverse acc, Token {tokenType = NewlineToken, lexeme = "\\n", position = dedentPos} : restTokens)
        _ -> do
          (nextCase, afterNext) <- parseCaseClause parseExprFn parseSuiteFn restAfterNewline
          parseCaseTail parseExprFn parseSuiteFn (nextCase : acc) afterNext

    parseCaseClause parseExprFn parseSuiteFn (Token CaseToken _ casePos : ts) = do
      (patternExpr, afterPattern) <- parsePattern (ParsePatternConfig {parsePatternExpr = parseExprFn}) ts
      (guardExpr, afterGuard) <-
        case afterPattern of
          Token IfToken _ _ : afterIf -> do
            (guardValue, guardRest) <- parseExprFn afterIf
            Right (Just guardValue, guardRest)
          _ -> Right (Nothing, afterPattern)
      case afterGuard of
        Token ColonToken _ _ : afterColon -> do
          (suite, finalRest) <- parseSuiteFn afterColon
          Right ((patternExpr, guardExpr, suite, casePos), finalRest)
        tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
        _ -> Left (ExpectedExpression {parseErrorPosition = Position 0 0})
    parseCaseClause _ _ (tok : _) = Left (ExpectedExpression {parseErrorPosition = position tok})
    parseCaseClause _ _ _ = Left (ExpectedExpression {parseErrorPosition = Position 0 0})

    consumeNewline (Token NewlineToken _ _ : restTokens) = Right restTokens
    consumeNewline (Token _ _ pos' : _) = Left (ExpectedNewlineAfterStatement {parseErrorPosition = pos'})
    consumeNewline [] = Left (ExpectedNewlineAfterStatement {parseErrorPosition = Position 0 0})
