module PythonHS.Parser.ParseLambdaExpr (parseLambdaExpr) where

import qualified Data.Set as Set
import PythonHS.AST.Expr (Expr (LambdaDefaultsExpr, LambdaExpr))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (AssignToken, ColonToken, CommaToken, IdentifierToken, LambdaToken))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))

parseLambdaExpr :: ([Token] -> Either ParseError (Expr, [Token])) -> [Token] -> Either ParseError (Expr, [Token])
parseLambdaExpr parseFallback (Token LambdaToken _ pos : rest) = do
  (params, defaults, afterParams) <- parseLambdaParameters False Set.empty rest
  case afterParams of
    Token ColonToken _ _ : afterColon -> do
      (bodyExpr, afterBody) <- parseLambdaExpr parseFallback afterColon
      if null defaults
        then Right (LambdaExpr params bodyExpr pos, afterBody)
        else Right (LambdaDefaultsExpr params defaults bodyExpr pos, afterBody)
    Token _ _ tokPos : _ -> Left (ExpectedExpression tokPos)
    _ -> Left (ExpectedExpression (Position 0 0))
  where
    parseLambdaParameters _ _ colonTokens@(Token ColonToken _ _ : _) =
      Right ([], [], colonTokens)
    parseLambdaParameters _ seenNames (Token IdentifierToken name namePos : Token AssignToken _ _ : restTokens) = do
      if Set.member name seenNames
        then Left (ExpectedExpression namePos)
        else do
          (defaultExpr, afterDefault) <- parseLambdaExpr parseFallback restTokens
          parseLambdaTail True (Set.insert name seenNames) [name] [(name, defaultExpr)] afterDefault
    parseLambdaParameters seenDefault seenNames (Token IdentifierToken name namePos : restTokens) = do
      if seenDefault || Set.member name seenNames
        then Left (ExpectedExpression namePos)
        else parseLambdaTail seenDefault (Set.insert name seenNames) [name] [] restTokens
    parseLambdaParameters _ _ (tok : _) = Left (ExpectedExpression (position tok))
    parseLambdaParameters _ _ _ = Left (ExpectedExpression (Position 0 0))

    parseLambdaTail seenDefault seenNames params defaults (Token CommaToken _ _ : restTokens) = do
      (otherParams, otherDefaults, afterParams) <- parseLambdaParameters seenDefault seenNames restTokens
      Right (params ++ otherParams, defaults ++ otherDefaults, afterParams)
    parseLambdaTail _ _ params defaults tokens = Right (params, defaults, tokens)
parseLambdaExpr parseFallback tokens = parseFallback tokens
