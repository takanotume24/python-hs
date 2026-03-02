module PythonHS.Parser.ParseParameters (parseParameters) where

import qualified Data.Set as Set
import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (AssignToken, CommaToken, IdentifierToken, RParenToken))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))

parseParameters :: ([Token] -> Either ParseError (Expr, [Token])) -> [Token] -> Either ParseError ([String], [(String, Expr)], [Token])
parseParameters parseExpr = parseParametersWithState False Set.empty
  where
    parseParametersWithState _ _ (Token RParenToken _ _ : rest) = Right ([], [], rest)
    parseParametersWithState seenDefault seenNames ts = do
      ((paramName, paramDefault, paramPos), afterParam) <- parseSingleParameter ts
      if seenDefault && paramDefault == Nothing
        then Left (ExpectedExpression paramPos)
        else if Set.member paramName seenNames
          then Left (ExpectedExpression paramPos)
          else case afterParam of
            Token RParenToken _ _ : rest ->
              Right ([paramName], paramDefaultPair paramName paramDefault, rest)
            Token CommaToken _ _ : rest -> do
              (otherParams, otherDefaults, after) <- parseParametersWithState (seenDefault || paramDefault /= Nothing) (Set.insert paramName seenNames) rest
              Right (paramName : otherParams, paramDefaultPair paramName paramDefault ++ otherDefaults, after)
            tok : _ -> Left (ExpectedExpression (position tok))
            _ -> Left (ExpectedExpression (Position 0 0))

    parseSingleParameter (Token IdentifierToken p pPos : Token AssignToken _ _ : rest) = do
      (defaultExpr, afterDefault) <- parseExpr rest
      Right ((p, Just defaultExpr, pPos), afterDefault)
    parseSingleParameter (Token IdentifierToken p pPos : rest) = Right ((p, Nothing, pPos), rest)
    parseSingleParameter (tok : _) = Left (ExpectedExpression (position tok))
    parseSingleParameter _ = Left (ExpectedExpression (Position 0 0))

    paramDefaultPair _ Nothing = []
    paramDefaultPair name (Just defaultExpr) = [(name, defaultExpr)]
