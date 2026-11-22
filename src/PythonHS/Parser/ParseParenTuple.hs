module PythonHS.Parser.ParseParenTuple (parseParenTuple) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (CommaToken, RParenToken, StarToken))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseParenTupleConfig (ParseParenTupleConfig (..))
import PythonHS.Parser.ParseResult (ParseResult (..))

parseParenTuple ::
  ParseParenTupleConfig ->
  Either ParseError (ParseResult Expr)
parseParenTuple config =
  let parseExpr = parseParenTupleExpr config
      parenPos = parseParenTuplePos config
      rest = parseParenTupleTokenStream config
   in case rest of
        Token RParenToken _ _ : rest' -> Right (ParseResult {parseResultValue = TupleExpr {tupleExprItems = [], tupleExprPos = parenPos}, parseResultRemaining = rest'})
        _ -> do
          firstResult <- parseElement parseExpr rest
          let firstExpr = parseResultValue firstResult
              afterFirst = parseResultRemaining firstResult
          parseTupleOrGrouped parseExpr parenPos firstExpr afterFirst
  where
    parseElement parseExprFn (Token StarToken _ starPos : restTokens) = do
      valueResult <- parseExprFn restTokens
      Right (ParseResult {parseResultValue = StarArgExpr {starArgExprValue = parseResultValue valueResult, starArgExprPos = starPos}, parseResultRemaining = parseResultRemaining valueResult})
    parseElement parseExprFn tokensToParse = parseExprFn tokensToParse

    parseTupleOrGrouped parseExpr parenPos firstExpr afterFirst =
      case afterFirst of
        Token CommaToken _ _ : restTokens ->
          parseTupleTail parseExpr parenPos [firstExpr] restTokens
        Token RParenToken _ _ : restTokens ->
          Right (ParseResult {parseResultValue = firstExpr, parseResultRemaining = restTokens})
        tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
        _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})

    parseTupleTail parseExpr parenPos exprs tokenStream =
      case tokenStream of
        Token RParenToken _ _ : restTokens ->
          Right (ParseResult {parseResultValue = TupleExpr {tupleExprItems = exprs, tupleExprPos = parenPos}, parseResultRemaining = restTokens})
        _ -> do
          nextResult <- parseElement parseExpr tokenStream
          let nextExpr = parseResultValue nextResult
              afterNext = parseResultRemaining nextResult
          case afterNext of
            Token CommaToken _ _ : restTokens -> parseTupleTail parseExpr parenPos (exprs ++ [nextExpr]) restTokens
            Token RParenToken _ _ : restTokens -> Right (ParseResult {parseResultValue = TupleExpr {tupleExprItems = exprs ++ [nextExpr], tupleExprPos = parenPos}, parseResultRemaining = restTokens})
            tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
