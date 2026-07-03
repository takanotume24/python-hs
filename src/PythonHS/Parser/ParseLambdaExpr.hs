module PythonHS.Parser.ParseLambdaExpr (parseLambdaExpr) where

import qualified Data.Set as Set
import PythonHS.AST.Expr (Expr (LambdaDefaultsExpr, LambdaExpr))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (AssignToken, ColonToken, CommaToken, IdentifierToken, LambdaToken))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))
import PythonHS.Parser.ParseLambdaExprConfig (ParseLambdaExprConfig (..))

parseLambdaExpr :: ParseLambdaExprConfig -> Either ParseError (Expr, [Token])
parseLambdaExpr config =
  let parseFallback = lambdaExprFallback config
      parseLambdaParameters seenDefault seenNames tokens =
        case tokens of
          colonTokens@(Token ColonToken _ _ : _) ->
            Right ([], [], colonTokens)
          Token IdentifierToken name namePos : Token AssignToken _ _ : restTokens ->
            if Set.member name seenNames
              then Left (ExpectedExpression namePos)
              else do
                (defaultExpr, afterDefault) <- parseLambdaExpr (ParseLambdaExprConfig parseFallback restTokens)
                parseLambdaTail True (Set.insert name seenNames) [name] [(name, defaultExpr)] afterDefault
          Token IdentifierToken name namePos : restTokens ->
            if seenDefault || Set.member name seenNames
              then Left (ExpectedExpression namePos)
              else parseLambdaTail False (Set.insert name seenNames) [name] [] restTokens
          tok : _ -> Left (ExpectedExpression (position tok))
          _ -> Left (ExpectedExpression (Position 0 0))
      parseLambdaTail seenDefault seenNames accParams accDefaults tokens =
        case tokens of
          Token CommaToken _ _ : restTokens -> do
            (otherParams, otherDefaults, afterParams) <- parseLambdaParameters seenDefault seenNames restTokens
            Right (accParams ++ otherParams, accDefaults ++ otherDefaults, afterParams)
          _ -> Right (accParams, accDefaults, tokens)
   in case lambdaExprTokens config of
        Token LambdaToken _ pos : rest -> do
          (params, defaults, afterParams) <- parseLambdaParameters False Set.empty rest
          case afterParams of
            Token ColonToken _ _ : afterColon -> do
              (bodyExpr, afterBody) <- parseLambdaExpr (ParseLambdaExprConfig parseFallback afterColon)
              if null defaults
                then Right (LambdaExpr params bodyExpr pos, afterBody)
                else Right (LambdaDefaultsExpr params defaults bodyExpr pos, afterBody)
            Token _ _ tokPos : _ -> Left (ExpectedExpression tokPos)
            _ -> Left (ExpectedExpression (Position 0 0))
        tokens -> parseFallback tokens
