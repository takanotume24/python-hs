module PythonHS.Parser.ParseMatchStmt (parseMatchStmt) where

import PythonHS.AST.MatchCase (MatchCase (..))
import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType (TokenType (CaseToken, ColonToken, DedentToken, IfToken, IndentToken, NewlineToken))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseExprNoBitwiseOr (parseExprNoBitwiseOr)
import PythonHS.Parser.ParseMatchStmtConfig (ParseMatchStmtConfig (..))
import PythonHS.Parser.ParsePattern (parsePattern)
import PythonHS.Parser.ParsePatternConfig (ParsePatternConfig (..))
import PythonHS.Parser.ParseResult (ParseResult (..))

parseMatchStmt ::
  ParseMatchStmtConfig ->
  Either ParseError (ParseResult Stmt)
parseMatchStmt config = do
  let parseExprFn = parseMatchStmtExpr config
      parseSuiteFn = parseMatchStmtSuite config
      pos = parseMatchStmtPos config
      rest = parseMatchStmtTokenStream config
  subjectResult <- parseExprFn rest
  let subjectExpr = parseResultValue subjectResult
      afterSubject = parseResultRemaining subjectResult
  case afterSubject of
    Token ColonToken _ _ : Token NewlineToken _ _ : Token IndentToken _ _ : afterIndent -> do
      casesResult <- parseCaseClauses parseExprFn parseSuiteFn afterIndent
      let cases = parseResultValue casesResult
          afterCases = parseResultRemaining casesResult
      Right (ParseResult {parseResultValue = MatchStmt {matchStmtSubject = subjectExpr, matchStmtCases = cases, matchStmtPos = pos}, parseResultRemaining = afterCases})
    Token ColonToken _ _ : _ -> Left (ExpectedExpression {parseErrorPosition = pos})
    tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
    _ -> Left (ExpectedExpression {parseErrorPosition = Position 0 0})
  where
    parseCaseClauses _ _ (Token DedentToken _ dedentPos : _) =
      Left (ExpectedExpression {parseErrorPosition = dedentPos})
    parseCaseClauses parseExprFn parseSuiteFn ts = do
      firstResult <- parseCaseClause parseExprFn parseSuiteFn ts
      let firstCase = parseResultValue firstResult
          afterFirst = parseResultRemaining firstResult
      parseCaseTail parseExprFn parseSuiteFn [firstCase] afterFirst

    parseCaseTail _ _ acc (Token DedentToken _ dedentPos : restTokens) =
      Right (ParseResult {parseResultValue = reverse acc, parseResultRemaining = Token {tokenType = NewlineToken, lexeme = "\\n", position = dedentPos} : restTokens})
    parseCaseTail parseExprFn parseSuiteFn acc ts = do
      restAfterNewline <- consumeNewline ts
      case restAfterNewline of
        Token DedentToken _ dedentPos : restTokens ->
          Right (ParseResult {parseResultValue = reverse acc, parseResultRemaining = Token {tokenType = NewlineToken, lexeme = "\\n", position = dedentPos} : restTokens})
        _ -> do
          nextResult <- parseCaseClause parseExprFn parseSuiteFn restAfterNewline
          let nextCase = parseResultValue nextResult
              afterNext = parseResultRemaining nextResult
          parseCaseTail parseExprFn parseSuiteFn (nextCase : acc) afterNext

    parseCaseClause parseExprFn parseSuiteFn (Token CaseToken _ casePos : ts) = do
      patternResult <- parsePattern (ParsePatternConfig {parsePatternExpr = parseExprNoBitwiseOr, parsePatternTokenStream = ts})
      let patternExpr = parseResultValue patternResult
          afterPattern = parseResultRemaining patternResult
      guardResult <-
        case afterPattern of
          Token IfToken _ _ : afterIf -> do
            guardValueResult <- parseExprFn afterIf
            let guardValue = parseResultValue guardValueResult
                guardRest = parseResultRemaining guardValueResult
            Right (ParseResult {parseResultValue = Just guardValue, parseResultRemaining = guardRest})
          _ -> Right (ParseResult {parseResultValue = Nothing, parseResultRemaining = afterPattern})
      let guardExpr = parseResultValue guardResult
          afterGuard = parseResultRemaining guardResult
      case afterGuard of
        Token ColonToken _ _ : afterColon -> do
          suiteResult <- parseSuiteFn afterColon
          let suite = parseResultValue suiteResult
              finalRest = parseResultRemaining suiteResult
          Right (ParseResult {parseResultValue = MatchCase {matchCasePattern = patternExpr, matchCaseGuard = guardExpr, matchCaseBody = suite, matchCasePos = casePos}, parseResultRemaining = finalRest})
        tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
        _ -> Left (ExpectedExpression {parseErrorPosition = Position 0 0})
    parseCaseClause _ _ (tok : _) = Left (ExpectedExpression {parseErrorPosition = position tok})
    parseCaseClause _ _ _ = Left (ExpectedExpression {parseErrorPosition = Position 0 0})

    consumeNewline (Token NewlineToken _ _ : restTokens) = Right restTokens
    consumeNewline (Token _ _ pos' : _) = Left (ExpectedNewlineAfterStatement {parseErrorPosition = pos'})
    consumeNewline [] = Left (ExpectedNewlineAfterStatement {parseErrorPosition = Position 0 0})
