module PythonHS.Parser.ParseSubscriptExpr (parseSubscriptExpr) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType (TokenType (ColonToken, RBracketToken))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseResult (ParseResult (..))
import PythonHS.Parser.ParseSubscriptExprConfig (ParseSubscriptExprConfig (..))

parseSubscriptExpr ::
  ParseSubscriptExprConfig ->
  Either ParseError (ParseResult Expr)
parseSubscriptExpr config =
  let parseExpr = parseSubscriptExprFn config
      receiverExpr = parseSubscriptExprReceiver config
      subscriptPos = parseSubscriptExprPos config
      ts = parseSubscriptExprTokenStream config
   in case ts of
        Token ColonToken _ _ : rest -> parseSlice Nothing rest
        Token RBracketToken _ _ : _ -> Left (ExpectedExpression {parseErrorPosition = subscriptPos})
        _ -> do
          firstResult <- parseExpr ts
          let firstExpr = parseResultValue firstResult
              afterFirst = parseResultRemaining firstResult
          case afterFirst of
            Token RBracketToken _ _ : rest -> Right (ParseResult {parseResultValue = IndexExpr {indexExprBase = receiverExpr, indexExprIndex = firstExpr, indexExprPos = subscriptPos}, parseResultRemaining = rest})
            Token ColonToken _ _ : rest -> parseSlice (Just firstExpr) rest
            tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
  where
    parseSlice maybeStart ts' =
      case ts' of
        Token RBracketToken _ _ : rest ->
          Right (ParseResult {parseResultValue = SliceExpr {sliceExprBase = parseSubscriptExprReceiver config, sliceExprStart = maybeStart, sliceExprEnd = Nothing, sliceExprPos = parseSubscriptExprPos config}, parseResultRemaining = rest})
        _ -> do
          endResult <- parseSubscriptExprFn config ts'
          let endExpr = parseResultValue endResult
              afterEnd = parseResultRemaining endResult
          case afterEnd of
            Token RBracketToken _ _ : rest ->
              Right (ParseResult {parseResultValue = SliceExpr {sliceExprBase = parseSubscriptExprReceiver config, sliceExprStart = maybeStart, sliceExprEnd = Just endExpr, sliceExprPos = parseSubscriptExprPos config}, parseResultRemaining = rest})
            tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
