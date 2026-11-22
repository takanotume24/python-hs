module PythonHS.Parser.ParseListDisplay (parseListDisplay) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (CommaToken, ForToken, RBracketToken, StarToken))
import PythonHS.Parser.DesugarStarredList (desugarStarredList)
import PythonHS.Parser.ParseComprehensionTail (parseComprehensionTail)
import PythonHS.Parser.ParseComprehensionTailConfig (ParseComprehensionTailConfig (..))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseListDisplayConfig (ParseListDisplayConfig (..))
import PythonHS.Parser.ParseResult (ParseResult (..))

parseListDisplay :: ParseListDisplayConfig -> Either ParseError (ParseResult Expr)
parseListDisplay config =
  case parseListDisplayTokenStream config of
    Token RBracketToken _ _ : rest ->
      Right (ParseResult {parseResultValue = ListExpr {listExprItems = [], listExprPos = parseListDisplayPos config}, parseResultRemaining = rest})
    ts -> do
      firstResult <- parseListElement ts
      let firstExpr = parseResultValue firstResult
          afterFirst = parseResultRemaining firstResult
      case afterFirst of
        forTokens@(Token ForToken _ _ : _) ->
          parseComprehensionTail
            ParseComprehensionTailConfig
              { parseComprehensionTailExpr = parseListDisplayExpr config,
                parseComprehensionTailValueExpr = firstExpr,
                parseComprehensionTailListPos = parseListDisplayPos config,
                parseComprehensionTailClauses = [],
                parseComprehensionTailTokenStream = forTokens
              }
        _ -> parseListTail [firstExpr] afterFirst
  where
    parseListElement (Token StarToken _ starPos : rest) = do
      valueResult <- parseListDisplayExpr config rest
      Right (ParseResult {parseResultValue = StarArgExpr {starArgExprValue = parseResultValue valueResult, starArgExprPos = starPos}, parseResultRemaining = parseResultRemaining valueResult})
    parseListElement ts = parseListDisplayExpr config ts

    parseListTail exprs (Token CommaToken _ _ : rest) =
      case rest of
        Token RBracketToken _ _ : afterBracket -> Right (ParseResult {parseResultValue = desugarStarredList (parseListDisplayPos config) exprs, parseResultRemaining = afterBracket})
        _ -> do
          nextResult <- parseListElement rest
          let nextExpr = parseResultValue nextResult
              afterNext = parseResultRemaining nextResult
          parseListTail (exprs ++ [nextExpr]) afterNext
    parseListTail exprs (Token RBracketToken _ _ : rest) =
      Right (ParseResult {parseResultValue = desugarStarredList (parseListDisplayPos config) exprs, parseResultRemaining = rest})
    parseListTail _ (tok : _) = Left (ExpectedExpression {parseErrorPosition = position tok})
    parseListTail _ _ = Left (ExpectedExpression {parseErrorPosition = Position 0 0})
