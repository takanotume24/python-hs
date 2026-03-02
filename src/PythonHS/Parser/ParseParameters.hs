module PythonHS.Parser.ParseParameters (parseParameters) where

import qualified Data.Set as Set
import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (AssignToken, ColonToken, CommaToken, IdentifierToken, RParenToken, StarToken))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))

parseParameters :: ([Token] -> Either ParseError (Expr, [Token])) -> [Token] -> Either ParseError ([String], [(String, Expr)], [Token])
parseParameters parseExpr = parseParametersWithState False False False Set.empty
  where
    parseParametersWithState _ _ _ _ (Token RParenToken _ _ : rest) = Right ([], [], rest)
    parseParametersWithState seenDefault seenVarArg seenKwArg seenNames ts = do
      ((paramName, paramDefault, paramPos), afterParam) <- parseSingleParameter ts
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
        else
          case afterParam of
            Token RParenToken _ _ : rest ->
              Right ([paramName], paramDefaultPair paramName paramDefault, rest)
            Token CommaToken _ _ : rest -> do
              (otherParams, otherDefaults, after) <-
                parseParametersWithState
                  (seenDefault || paramDefault /= Nothing)
                  (seenVarArg || isVarArg)
                  (seenKwArg || isKwArg)
                  (Set.insert normalizedName seenNames)
                  rest
              Right (paramName : otherParams, paramDefaultPair paramName paramDefault ++ otherDefaults, after)
            tok : _ -> Left (ExpectedExpression (position tok))
            _ -> Left (ExpectedExpression (Position 0 0))

    parseSingleParameter (Token StarToken _ pPos : Token StarToken _ _ : Token IdentifierToken p _ : rest) =
      Right (("**" ++ p, Nothing, pPos), rest)
    parseSingleParameter (Token StarToken _ pPos : Token IdentifierToken p _ : rest) =
      Right (("*" ++ p, Nothing, pPos), rest)
    parseSingleParameter (Token IdentifierToken p pPos : Token AssignToken _ _ : rest) = do
      (defaultExpr, afterDefault) <- parseExpr rest
      Right ((p, Just defaultExpr, pPos), afterDefault)
    parseSingleParameter (Token IdentifierToken p pPos : Token ColonToken _ _ : rest) = do
      (_, afterAnnotation) <- parseExpr rest
      case afterAnnotation of
        Token AssignToken _ _ : afterAssign -> do
          (defaultExpr, afterDefault) <- parseExpr afterAssign
          Right ((p, Just defaultExpr, pPos), afterDefault)
        _ -> Right ((p, Nothing, pPos), afterAnnotation)
    parseSingleParameter (Token IdentifierToken p pPos : rest) = Right ((p, Nothing, pPos), rest)
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
    paramDefaultPair name (Just defaultExpr) = [(name, defaultExpr)]
