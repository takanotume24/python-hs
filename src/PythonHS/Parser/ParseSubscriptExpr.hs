module PythonHS.Parser.ParseSubscriptExpr (parseSubscriptExpr) where

import PythonHS.AST.Expr (Expr (IndexExpr, SliceExpr))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (ColonToken, RBracketToken))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))

parseSubscriptExpr ::
  ([Token] -> Either ParseError (Expr, [Token])) ->
  Expr ->
  Position ->
  [Token] ->
  Either ParseError (Expr, [Token])
parseSubscriptExpr parseExpr receiverExpr subscriptPos ts =
  case ts of
    Token ColonToken _ _ : rest -> parseSlice Nothing rest
    Token RBracketToken _ _ : _ -> Left (ExpectedExpression subscriptPos)
    _ -> do
      (firstExpr, afterFirst) <- parseExpr ts
      case afterFirst of
        Token RBracketToken _ _ : rest -> Right (IndexExpr receiverExpr firstExpr subscriptPos, rest)
        Token ColonToken _ _ : rest -> parseSlice (Just firstExpr) rest
        tok : _ -> Left (ExpectedExpression (position tok))
        _ -> Left (ExpectedExpression (Position 0 0))
  where
    parseSlice maybeStart ts' =
      case ts' of
        Token RBracketToken _ _ : rest ->
          Right (SliceExpr receiverExpr maybeStart Nothing subscriptPos, rest)
        _ -> do
          (endExpr, afterEnd) <- parseExpr ts'
          case afterEnd of
            Token RBracketToken _ _ : rest ->
              Right (SliceExpr receiverExpr maybeStart (Just endExpr) subscriptPos, rest)
            tok : _ -> Left (ExpectedExpression (position tok))
            _ -> Left (ExpectedExpression (Position 0 0))
