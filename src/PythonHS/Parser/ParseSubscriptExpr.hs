module PythonHS.Parser.ParseSubscriptExpr (parseSubscriptExpr) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType (TokenType (ColonToken, RBracketToken))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseSubscriptExprConfig (ParseSubscriptExprConfig (..))

parseSubscriptExpr ::
  ParseSubscriptExprConfig ->
  [Token] ->
  Either ParseError (Expr, [Token])
parseSubscriptExpr config ts =
  let parseExpr = parseSubscriptExprFn config
      receiverExpr = parseSubscriptExprReceiver config
      subscriptPos = parseSubscriptExprPos config
   in case ts of
        Token ColonToken _ _ : rest -> parseSlice Nothing rest
        Token RBracketToken _ _ : _ -> Left (ExpectedExpression {parseErrorPosition = subscriptPos})
        _ -> do
          (firstExpr, afterFirst) <- parseExpr ts
          case afterFirst of
            Token RBracketToken _ _ : rest -> Right (IndexExpr {indexExprBase = receiverExpr, indexExprIndex = firstExpr, indexExprPos = subscriptPos}, rest)
            Token ColonToken _ _ : rest -> parseSlice (Just firstExpr) rest
            tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
  where
    parseSlice maybeStart ts' =
      case ts' of
        Token RBracketToken _ _ : rest ->
          Right (SliceExpr {sliceExprBase = parseSubscriptExprReceiver config, sliceExprStart = maybeStart, sliceExprEnd = Nothing, sliceExprPos = parseSubscriptExprPos config}, rest)
        _ -> do
          (endExpr, afterEnd) <- parseSubscriptExprFn config ts'
          case afterEnd of
            Token RBracketToken _ _ : rest ->
              Right (SliceExpr {sliceExprBase = parseSubscriptExprReceiver config, sliceExprStart = maybeStart, sliceExprEnd = Just endExpr, sliceExprPos = parseSubscriptExprPos config}, rest)
            tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
