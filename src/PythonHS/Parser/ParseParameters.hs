module PythonHS.Parser.ParseParameters (parseParameters) where

import Data.Set qualified as Set
import PythonHS.AST.Expr (Expr)
import PythonHS.AST.FunctionDefault (FunctionDefault (..))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (AssignToken, ColonToken, CommaToken, IdentifierToken, RParenToken, StarToken))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))
import PythonHS.Parser.ParseParametersResult (ParseParametersResult (..))
import PythonHS.Parser.ParseResult (ParseResult (..))
import PythonHS.Parser.ParseSingleParameterResult (ParseSingleParameterResult (..))

parseParameters :: ([Token] -> Either ParseError (ParseResult Expr)) -> [Token] -> Either ParseError (ParseResult ParseParametersResult)
parseParameters parseExpr = parseParametersWithState False False False Set.empty
  where
    parseParametersWithState _ _ _ _ (Token RParenToken _ _ : rest) = Right (ParseResult {parseResultValue = ParseParametersResult {parseParametersResultParams = [], parseParametersResultDefaults = []}, parseResultRemaining = rest})
    parseParametersWithState seenDefault seenVarArg seenKwArg seenNames ts = do
      paramResult <- parseSingleParameter ts
      let paramRecord = parseResultValue paramResult
          paramName = parseSingleParameterResultName paramRecord
          paramDefault = parseSingleParameterResultDefault paramRecord
          paramPos = parseSingleParameterResultPos paramRecord
          afterParam = parseResultRemaining paramResult
      let normalizedName = canonicalParamName paramName
          isVarArg = isStarParameter paramName
          isKwArg = isKwStarParameter paramName
          hasInvalidOrder =
            (seenDefault && paramDefault == Nothing && not isVarArg && not isKwArg)
              || (seenVarArg && not isKwArg)
              || seenKwArg
          hasDuplicate = Set.member normalizedName seenNames
      if hasInvalidOrder || hasDuplicate
        then Left (ExpectedExpression paramPos)
        else case afterParam of
          Token RParenToken _ _ : rest ->
            Right (ParseResult {parseResultValue = ParseParametersResult {parseParametersResultParams = [paramName], parseParametersResultDefaults = paramDefaultPair paramName paramDefault}, parseResultRemaining = rest})
          Token CommaToken _ _ : rest -> do
            paramsResult <-
              parseParametersWithState
                (seenDefault || paramDefault /= Nothing)
                (seenVarArg || isVarArg)
                (seenKwArg || isKwArg)
                (Set.insert normalizedName seenNames)
                rest
            let otherParams = parseParametersResultParams (parseResultValue paramsResult)
                otherDefaults = parseParametersResultDefaults (parseResultValue paramsResult)
                after = parseResultRemaining paramsResult
            Right (ParseResult {parseResultValue = ParseParametersResult {parseParametersResultParams = paramName : otherParams, parseParametersResultDefaults = paramDefaultPair paramName paramDefault ++ otherDefaults}, parseResultRemaining = after})
          tok : _ -> Left (ExpectedExpression (position tok))
          _ -> Left (ExpectedExpression (Position 0 0))

    parseSingleParameter (Token StarToken _ pPos : Token StarToken _ _ : Token IdentifierToken p _ : rest) =
      Right (ParseResult {parseResultValue = ParseSingleParameterResult {parseSingleParameterResultName = "**" ++ p, parseSingleParameterResultDefault = Nothing, parseSingleParameterResultPos = pPos}, parseResultRemaining = rest})
    parseSingleParameter (Token StarToken _ pPos : Token IdentifierToken p _ : rest) =
      Right (ParseResult {parseResultValue = ParseSingleParameterResult {parseSingleParameterResultName = "*" ++ p, parseSingleParameterResultDefault = Nothing, parseSingleParameterResultPos = pPos}, parseResultRemaining = rest})
    parseSingleParameter (Token IdentifierToken p pPos : Token AssignToken _ _ : rest) = do
      defaultResult <- parseExpr rest
      let defaultExpr = parseResultValue defaultResult
          afterDefault = parseResultRemaining defaultResult
      Right (ParseResult {parseResultValue = ParseSingleParameterResult {parseSingleParameterResultName = p, parseSingleParameterResultDefault = Just defaultExpr, parseSingleParameterResultPos = pPos}, parseResultRemaining = afterDefault})
    parseSingleParameter (Token IdentifierToken p pPos : Token ColonToken _ _ : rest) = do
      annotationResult <- parseExpr rest
      let afterAnnotation = parseResultRemaining annotationResult
      case afterAnnotation of
        Token AssignToken _ _ : afterAssign -> do
          defaultResult <- parseExpr afterAssign
          let defaultExpr = parseResultValue defaultResult
              afterDefault = parseResultRemaining defaultResult
          Right (ParseResult {parseResultValue = ParseSingleParameterResult {parseSingleParameterResultName = p, parseSingleParameterResultDefault = Just defaultExpr, parseSingleParameterResultPos = pPos}, parseResultRemaining = afterDefault})
        _ -> Right (ParseResult {parseResultValue = ParseSingleParameterResult {parseSingleParameterResultName = p, parseSingleParameterResultDefault = Nothing, parseSingleParameterResultPos = pPos}, parseResultRemaining = afterAnnotation})
    parseSingleParameter (Token IdentifierToken p pPos : rest) = Right (ParseResult {parseResultValue = ParseSingleParameterResult {parseSingleParameterResultName = p, parseSingleParameterResultDefault = Nothing, parseSingleParameterResultPos = pPos}, parseResultRemaining = rest})
    parseSingleParameter (tok : _) = Left (ExpectedExpression (position tok))
    parseSingleParameter _ = Left (ExpectedExpression (Position 0 0))

    canonicalParamName name
      | isKwStarParameter name = drop 2 name
      | isStarParameter name = drop 1 name
      | otherwise = name

    isStarParameter ('*' : c : _) = c /= '*'
    isStarParameter _ = False

    isKwStarParameter ('*' : '*' : _) = True
    isKwStarParameter _ = False

    paramDefaultPair _ Nothing = []
    paramDefaultPair name (Just defaultExpr) = [FunctionDefault {functionDefaultParam = name, functionDefaultExpr = defaultExpr}]
