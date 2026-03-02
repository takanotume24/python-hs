module PythonHS.Parser.ParseParenTuple (parseParenTuple) where

import PythonHS.AST.Expr (Expr (TupleExpr))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (CommaToken, RParenToken))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))

parseParenTuple ::
  ([Token] -> Either ParseError (Expr, [Token])) ->
  Position ->
  [Token] ->
  Either ParseError (Expr, [Token])
parseParenTuple parseExpr parenPos rest =
  case rest of
    Token RParenToken _ _ : rest' -> Right (TupleExpr [] parenPos, rest')
    _ -> do
      (firstExpr, afterFirst) <- parseExpr rest
      parseTupleOrGrouped firstExpr afterFirst
  where
    parseTupleOrGrouped firstExpr afterFirst =
      case afterFirst of
        Token CommaToken _ _ : restTokens ->
          parseTupleTail [firstExpr] restTokens
        Token RParenToken _ _ : restTokens ->
          Right (firstExpr, restTokens)
        tok : _ -> Left (ExpectedExpression (position tok))
        _ -> Left (ExpectedExpression (Position 0 0))

    parseTupleTail exprs tokenStream =
      case tokenStream of
        Token RParenToken _ _ : restTokens ->
          Right (TupleExpr exprs parenPos, restTokens)
        _ -> do
          (nextExpr, afterNext) <- parseExpr tokenStream
          case afterNext of
            Token CommaToken _ _ : restTokens -> parseTupleTail (exprs ++ [nextExpr]) restTokens
            Token RParenToken _ _ : restTokens -> Right (TupleExpr (exprs ++ [nextExpr]) parenPos, restTokens)
            tok : _ -> Left (ExpectedExpression (position tok))
            _ -> Left (ExpectedExpression (Position 0 0))
