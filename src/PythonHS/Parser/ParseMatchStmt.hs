module PythonHS.Parser.ParseMatchStmt (parseMatchStmt) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt (MatchStmt), Stmt)
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (CaseToken, ColonToken, DedentToken, IfToken, IndentToken, NewlineToken))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression, ExpectedNewlineAfterStatement))
import PythonHS.Parser.ParsePattern (parsePattern)

parseMatchStmt ::
  ([Token] -> Either ParseError (Expr, [Token])) ->
  ([Token] -> Either ParseError ([Stmt], [Token])) ->
  Position ->
  [Token] ->
  Either ParseError (Stmt, [Token])
parseMatchStmt parseExpr parseSuite pos rest = do
  (subjectExpr, afterSubject) <- parseExpr rest
  case afterSubject of
    Token ColonToken _ _ : Token NewlineToken _ _ : Token IndentToken _ _ : afterIndent -> do
      (cases, afterCases) <- parseCaseClauses afterIndent
      Right (MatchStmt subjectExpr cases pos, afterCases)
    Token ColonToken _ _ : _ -> Left (ExpectedExpression pos)
    tok : _ -> Left (ExpectedExpression (position tok))
    _ -> Left (ExpectedExpression (Position 0 0))
  where
    parseCaseClauses (Token DedentToken _ dedentPos : _) =
      Left (ExpectedExpression dedentPos)
    parseCaseClauses ts = do
      (firstCase, afterFirst) <- parseCaseClause ts
      parseCaseTail [firstCase] afterFirst

    parseCaseTail acc (Token DedentToken _ dedentPos : restTokens) =
      Right (reverse acc, Token NewlineToken "\\n" dedentPos : restTokens)
    parseCaseTail acc ts = do
      restAfterNewline <- consumeNewline ts
      case restAfterNewline of
        Token DedentToken _ dedentPos : restTokens ->
          Right (reverse acc, Token NewlineToken "\\n" dedentPos : restTokens)
        _ -> do
          (nextCase, afterNext) <- parseCaseClause restAfterNewline
          parseCaseTail (nextCase : acc) afterNext

    parseCaseClause (Token CaseToken _ casePos : ts) = do
      (patternExpr, afterPattern) <- parsePattern parseExpr ts
      (guardExpr, afterGuard) <-
        case afterPattern of
          Token IfToken _ _ : afterIf -> do
            (guardValue, guardRest) <- parseExpr afterIf
            Right (Just guardValue, guardRest)
          _ -> Right (Nothing, afterPattern)
      case afterGuard of
        Token ColonToken _ _ : afterColon -> do
          (suite, finalRest) <- parseSuite afterColon
          Right ((patternExpr, guardExpr, suite, casePos), finalRest)
        tok : _ -> Left (ExpectedExpression (position tok))
        _ -> Left (ExpectedExpression (Position 0 0))
    parseCaseClause (tok : _) = Left (ExpectedExpression (position tok))
    parseCaseClause _ = Left (ExpectedExpression (Position 0 0))

    consumeNewline (Token NewlineToken _ _ : restTokens) = Right restTokens
    consumeNewline (Token _ _ pos' : _) = Left (ExpectedNewlineAfterStatement pos')
    consumeNewline [] = Left (ExpectedNewlineAfterStatement (Position 0 0))
