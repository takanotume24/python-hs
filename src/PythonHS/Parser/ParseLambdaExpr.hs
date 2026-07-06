module PythonHS.Parser.ParseLambdaExpr (parseLambdaExpr) where

import Data.Set qualified as Set
import PythonHS.AST.Expr (Expr (LambdaDefaultsExpr, LambdaExpr))
import PythonHS.AST.FunctionDefault (FunctionDefault (..))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (AssignToken, ColonToken, CommaToken, IdentifierToken, LambdaToken))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))
import PythonHS.Parser.ParseLambdaExprConfig (ParseLambdaExprConfig (..))
import PythonHS.Parser.ParseParametersResult (ParseParametersResult (..))
import PythonHS.Parser.ParseResult (ParseResult (..))

parseLambdaExpr :: ParseLambdaExprConfig -> Either ParseError (ParseResult Expr)
parseLambdaExpr config =
  let parseFallback = lambdaExprFallback config
      parseLambdaParameters seenDefault seenNames tokens =
        case tokens of
          colonTokens@(Token ColonToken _ _ : _) ->
            Right (ParseResult {parseResultValue = ParseParametersResult {parseParametersResultParams = [], parseParametersResultDefaults = []}, parseResultRemaining = colonTokens})
          Token IdentifierToken name namePos : Token AssignToken _ _ : restTokens ->
            if Set.member name seenNames
              then Left (ExpectedExpression namePos)
              else do
                defaultResult <- parseLambdaExpr (ParseLambdaExprConfig {lambdaExprFallback = parseFallback, lambdaExprTokens = restTokens})
                let defaultExpr = parseResultValue defaultResult
                    afterDefault = parseResultRemaining defaultResult
                parseLambdaTail True (Set.insert name seenNames) [name] [FunctionDefault {functionDefaultParam = name, functionDefaultExpr = defaultExpr}] afterDefault
          Token IdentifierToken name namePos : restTokens ->
            if seenDefault || Set.member name seenNames
              then Left (ExpectedExpression namePos)
              else parseLambdaTail False (Set.insert name seenNames) [name] [] restTokens
          tok : _ -> Left (ExpectedExpression (position tok))
          _ -> Left (ExpectedExpression (Position 0 0))
      parseLambdaTail seenDefault seenNames accParams accDefaults tokens =
        case tokens of
          Token CommaToken _ _ : restTokens -> do
            paramsResult <- parseLambdaParameters seenDefault seenNames restTokens
            let otherParams = parseParametersResultParams (parseResultValue paramsResult)
                otherDefaults = parseParametersResultDefaults (parseResultValue paramsResult)
                afterParams = parseResultRemaining paramsResult
            Right (ParseResult {parseResultValue = ParseParametersResult {parseParametersResultParams = accParams ++ otherParams, parseParametersResultDefaults = accDefaults ++ otherDefaults}, parseResultRemaining = afterParams})
          _ -> Right (ParseResult {parseResultValue = ParseParametersResult {parseParametersResultParams = accParams, parseParametersResultDefaults = accDefaults}, parseResultRemaining = tokens})
   in case lambdaExprTokens config of
        Token LambdaToken _ pos : rest -> do
          paramsResult <- parseLambdaParameters False Set.empty rest
          let params = parseParametersResultParams (parseResultValue paramsResult)
              defaults = parseParametersResultDefaults (parseResultValue paramsResult)
              afterParams = parseResultRemaining paramsResult
          case afterParams of
            Token ColonToken _ _ : afterColon -> do
              bodyResult <- parseLambdaExpr (ParseLambdaExprConfig {lambdaExprFallback = parseFallback, lambdaExprTokens = afterColon})
              let bodyExpr = parseResultValue bodyResult
                  afterBody = parseResultRemaining bodyResult
              if null defaults
                then Right (ParseResult {parseResultValue = LambdaExpr params bodyExpr pos, parseResultRemaining = afterBody})
                else Right (ParseResult {parseResultValue = LambdaDefaultsExpr params defaults bodyExpr pos, parseResultRemaining = afterBody})
            Token _ _ tokPos : _ -> Left (ExpectedExpression tokPos)
            _ -> Left (ExpectedExpression (Position 0 0))
        tokens -> parseFallback tokens
