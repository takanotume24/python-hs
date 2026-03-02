module PythonHS.Parser.ParseLambdaExpr (parseLambdaExpr) where

import PythonHS.AST.Expr (Expr (LambdaExpr))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (ColonToken, CommaToken, IdentifierToken, LambdaToken))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))

parseLambdaExpr :: ([Token] -> Either ParseError (Expr, [Token])) -> [Token] -> Either ParseError (Expr, [Token])
parseLambdaExpr parseFallback (Token LambdaToken _ pos : rest) = do
  (params, afterParams) <- parseLambdaParameters rest
  case afterParams of
    Token ColonToken _ _ : afterColon -> do
      (bodyExpr, afterBody) <- parseLambdaExpr parseFallback afterColon
      Right (LambdaExpr params bodyExpr pos, afterBody)
    Token _ _ tokPos : _ -> Left (ExpectedExpression tokPos)
    _ -> Left (ExpectedExpression (Position 0 0))
  where
    parseLambdaParameters colonTokens@(Token ColonToken _ _ : _) = Right ([], colonTokens)
    parseLambdaParameters ts = parseLambdaParametersTail [] ts

    parseLambdaParametersTail params (Token IdentifierToken name _ : Token CommaToken _ _ : restTokens) =
      parseLambdaParametersTail (params ++ [name]) restTokens
    parseLambdaParametersTail params (Token IdentifierToken name _ : restTokens) =
      Right (params ++ [name], restTokens)
    parseLambdaParametersTail params tokens = Right (params, tokens)
parseLambdaExpr parseFallback tokens = parseFallback tokens
