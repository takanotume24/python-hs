module PythonHS.Parser.ParseParenTuple (parseParenTuple) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (CommaToken, RParenToken))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseParenTupleConfig (ParseParenTupleConfig (..))

parseParenTuple ::
  ParseParenTupleConfig ->
  [Token] ->
  Either ParseError (Expr, [Token])
parseParenTuple config rest =
  let parseExpr = parseParenTupleExpr config
      parenPos = parseParenTuplePos config
   in case rest of
        Token RParenToken _ _ : rest' -> Right (TupleExpr {tupleExprItems = [], tupleExprPos = parenPos}, rest')
        _ -> do
          (firstExpr, afterFirst) <- parseExpr rest
          parseTupleOrGrouped parseExpr parenPos firstExpr afterFirst
  where
    parseTupleOrGrouped parseExpr parenPos firstExpr afterFirst =
      case afterFirst of
        Token CommaToken _ _ : restTokens ->
          parseTupleTail parseExpr parenPos [firstExpr] restTokens
        Token RParenToken _ _ : restTokens ->
          Right (firstExpr, restTokens)
        tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
        _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})

    parseTupleTail parseExpr parenPos exprs tokenStream =
      case tokenStream of
        Token RParenToken _ _ : restTokens ->
          Right (TupleExpr {tupleExprItems = exprs, tupleExprPos = parenPos}, restTokens)
        _ -> do
          (nextExpr, afterNext) <- parseExpr tokenStream
          case afterNext of
            Token CommaToken _ _ : restTokens -> parseTupleTail parseExpr parenPos (exprs ++ [nextExpr]) restTokens
            Token RParenToken _ _ : restTokens -> Right (TupleExpr {tupleExprItems = exprs ++ [nextExpr], tupleExprPos = parenPos}, restTokens)
            tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
