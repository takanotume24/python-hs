module PythonHS.Parser.ParseMatchStmt (parseMatchStmt) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt (MatchStmt), Stmt)
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (CaseToken, ColonToken, DedentToken, IfToken, IndentToken, NewlineToken))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression, ExpectedNewlineAfterStatement))
import PythonHS.Parser.ParseMatchStmtConfig (ParseMatchStmtConfig (..))
import PythonHS.Parser.ParsePattern (parsePattern)

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
      Right (MatchStmt subjectExpr cases pos, afterCases)
    Token ColonToken _ _ : _ -> Left (ExpectedExpression pos)
    tok : _ -> Left (ExpectedExpression (position tok))
    _ -> Left (ExpectedExpression (Position 0 0))
  where
    parseCaseClauses parseExprFn parseSuiteFn (Token DedentToken _ dedentPos : _) =
      Left (ExpectedExpression dedentPos)
    parseCaseClauses parseExprFn parseSuiteFn ts = do
      (firstCase, afterFirst) <- parseCaseClause parseExprFn parseSuiteFn ts
      parseCaseTail parseExprFn parseSuiteFn [firstCase] afterFirst

    parseCaseTail _ _ acc (Token DedentToken _ dedentPos : restTokens) =
      Right (reverse acc, Token NewlineToken "\\n" dedentPos : restTokens)
    parseCaseTail parseExprFn parseSuiteFn acc ts = do
      restAfterNewline <- consumeNewline ts
      case restAfterNewline of
        Token DedentToken _ dedentPos : restTokens ->
          Right (reverse acc, Token NewlineToken "\\n" dedentPos : restTokens)
        _ -> do
          (nextCase, afterNext) <- parseCaseClause parseExprFn parseSuiteFn restAfterNewline
          parseCaseTail parseExprFn parseSuiteFn (nextCase : acc) afterNext

    parseCaseClause parseExprFn parseSuiteFn (Token CaseToken _ casePos : ts) = do
      (patternExpr, afterPattern) <- parsePattern parseExprFn ts
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
        tok : _ -> Left (ExpectedExpression (position tok))
        _ -> Left (ExpectedExpression (Position 0 0))
    parseCaseClause _ _ (tok : _) = Left (ExpectedExpression (position tok))
    parseCaseClause _ _ _ = Left (ExpectedExpression (Position 0 0))

    consumeNewline (Token NewlineToken _ _ : restTokens) = Right restTokens
    consumeNewline (Token _ _ pos' : _) = Left (ExpectedNewlineAfterStatement pos')
    consumeNewline [] = Left (ExpectedNewlineAfterStatement (Position 0 0))
