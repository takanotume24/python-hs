module PythonHS.Parser.ParseExceptSuites (parseExceptSuites) where

import PythonHS.AST.ExceptSuite (ExceptSuite (..))
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType
  ( TokenType
      ( AsToken,
        ColonToken,
        CommaToken,
        ExceptToken,
        IdentifierToken,
        LParenToken,
        RParenToken,
        StarToken
      ),
  )
import PythonHS.Parser.DropLeadingNewlines (dropLeadingNewlines)
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseExceptSuitesConfig (ParseExceptSuitesConfig (..))
import PythonHS.Parser.ParseResult (ParseResult (..))

parseExceptSuites :: ParseExceptSuitesConfig -> Either ParseError (ParseResult [ExceptSuite Stmt])
parseExceptSuites config =
  let parseSuite = parseExceptSuitesSuite config
      ts = parseExceptSuitesTokenStream config
   in case ts of
        Token ExceptToken _ exceptPos : restAfterExcept -> do
          let afterStar =
                case restAfterExcept of
                  Token {tokenType = StarToken} : rest -> rest
                  _ -> restAfterExcept
          typeNamesResult <- parseExceptTypeNames afterStar
          let typeNames = parseResultValue typeNamesResult
              afterTypeNames = parseResultRemaining typeNamesResult
          aliasResult <- parseAlias afterTypeNames
          let maybeAlias = parseResultValue aliasResult
              aliasRemaining = parseResultRemaining aliasResult
          afterColon <- requireColon aliasRemaining
          suiteResult <- parseSuite afterColon
          let clause =
                ExceptSuite
                  { exceptSuiteType = typeNames,
                    exceptSuiteAlias = maybeAlias,
                    exceptSuiteBody = parseResultValue suiteResult,
                    exceptSuitePos = exceptPos
                  }
           in case parseExceptSuites (config {parseExceptSuitesTokenStream = dropLeadingNewlines (parseResultRemaining suiteResult)}) of
                Right suitesResult ->
                  Right (ParseResult {parseResultValue = clause : parseResultValue suitesResult, parseResultRemaining = parseResultRemaining suitesResult})
                Left _ ->
                  Right (ParseResult {parseResultValue = [clause], parseResultRemaining = parseResultRemaining suiteResult})
        Token _ _ pos' : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
        _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
  where
    requireColon tokens = case tokens of
      Token {tokenType = ColonToken} : afterColon -> Right afterColon
      Token {tokenType = _, position = pos} : _ -> Left (ExpectedExpression {parseErrorPosition = pos})
      _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})

    parseExceptTypeNames (Token {tokenType = IdentifierToken, lexeme = name} : rest) =
      Right (ParseResult {parseResultValue = Just [name], parseResultRemaining = rest})
    parseExceptTypeNames (Token {tokenType = LParenToken} : rest) =
      parseParenthesizedNames rest
    parseExceptTypeNames tokens = Right (ParseResult {parseResultValue = Nothing, parseResultRemaining = tokens})

    parseParenthesizedNames tokens = case tokens of
      Token {tokenType = IdentifierToken, lexeme = first} : rest -> parseParenNamesTail [first] rest
      Token {tokenType = _, position = pos} : _ -> Left (ExpectedExpression {parseErrorPosition = pos})
      _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})

    parseParenNamesTail acc tokens = case tokens of
      Token {tokenType = CommaToken} : Token {tokenType = IdentifierToken, lexeme = next} : rest ->
        parseParenNamesTail (next : acc) rest
      Token {tokenType = RParenToken} : rest ->
        Right (ParseResult {parseResultValue = Just (reverse acc), parseResultRemaining = rest})
      Token {tokenType = _, position = pos} : _ -> Left (ExpectedExpression {parseErrorPosition = pos})
      _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})

    parseAlias (Token {tokenType = AsToken} : Token {tokenType = IdentifierToken, lexeme = aliasName} : rest) =
      Right (ParseResult {parseResultValue = Just aliasName, parseResultRemaining = rest})
    parseAlias tokens = Right (ParseResult {parseResultValue = Nothing, parseResultRemaining = tokens})
