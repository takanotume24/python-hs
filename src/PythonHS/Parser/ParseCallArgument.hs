module PythonHS.Parser.ParseCallArgument (parseCallArgument) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.Lexer.Position (Position)
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (AssignToken, IdentifierToken, StarToken))
import PythonHS.Parser.ExprPos (exprPos)
import PythonHS.Parser.ParseCallArgumentConfig (ParseCallArgumentConfig (..))
import PythonHS.Parser.ParseCallArgumentResult (ParseCallArgumentResult (..))
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult (..))

parseCallArgument ::
  ParseCallArgumentConfig ->
  Either ParseError (ParseResult ParseCallArgumentResult)
parseCallArgument config =
  let parseExpr = parseCallArgumentExpr config
      tokenStream = parseCallArgumentTokenStream config
   in case tokenStream of
        Token IdentifierToken name namePos : Token AssignToken _ assignPos : rest -> do
          result <- parseExpr rest
          let valueExpr = parseResultValue result
              afterValue = parseResultRemaining result
          Right (ParseResult {parseResultValue = ParseCallArgumentResult {parseCallArgumentResultExpr = KeywordArgExpr {keywordArgExprName = name, keywordArgExprValue = valueExpr, keywordArgExprPos = namePos}, parseCallArgumentResultIsKeyword = True, parseCallArgumentResultPos = assignPos}, parseResultRemaining = afterValue})
        Token StarToken _ starPos : Token StarToken _ _ : rest -> do
          result <- parseExpr rest
          let valueExpr = parseResultValue result
              afterValue = parseResultRemaining result
          Right (ParseResult {parseResultValue = ParseCallArgumentResult {parseCallArgumentResultExpr = KwStarArgExpr {kwStarArgExprValue = valueExpr, kwStarArgExprPos = starPos}, parseCallArgumentResultIsKeyword = True, parseCallArgumentResultPos = starPos}, parseResultRemaining = afterValue})
        Token StarToken _ starPos : rest -> do
          result <- parseExpr rest
          let valueExpr = parseResultValue result
              afterValue = parseResultRemaining result
          Right (ParseResult {parseResultValue = ParseCallArgumentResult {parseCallArgumentResultExpr = StarArgExpr {starArgExprValue = valueExpr, starArgExprPos = starPos}, parseCallArgumentResultIsKeyword = False, parseCallArgumentResultPos = starPos}, parseResultRemaining = afterValue})
        _ -> do
          result <- parseExpr tokenStream
          let argExpr = parseResultValue result
              afterArg = parseResultRemaining result
          Right (ParseResult {parseResultValue = ParseCallArgumentResult {parseCallArgumentResultExpr = argExpr, parseCallArgumentResultIsKeyword = False, parseCallArgumentResultPos = exprPos argExpr}, parseResultRemaining = afterArg})
