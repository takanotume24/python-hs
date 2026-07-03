module PythonHS.Parser.ParseSubscriptExpr (parseSubscriptExpr) where

import PythonHS.AST.Expr (Expr (IndexExpr, SliceExpr))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (ColonToken, RBracketToken))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))
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
          Right (SliceExpr (parseSubscriptExprReceiver config) maybeStart Nothing (parseSubscriptExprPos config), rest)
        _ -> do
          (endExpr, afterEnd) <- parseSubscriptExprFn config ts'
          case afterEnd of
            Token RBracketToken _ _ : rest ->
              Right (SliceExpr (parseSubscriptExprReceiver config) maybeStart (Just endExpr) (parseSubscriptExprPos config), rest)
            tok : _ -> Left (ExpectedExpression (position tok))
            _ -> Left (ExpectedExpression (Position 0 0))
