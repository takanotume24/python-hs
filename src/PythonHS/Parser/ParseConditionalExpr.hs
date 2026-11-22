module PythonHS.Parser.ParseConditionalExpr (parseConditionalExpr) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (ElseToken, IfToken))
import PythonHS.Parser.ParseConditionalExprConfig (ParseConditionalExprConfig (..))
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult (..))

parseConditionalExpr :: ParseConditionalExprConfig -> Either ParseError (ParseResult Expr)
parseConditionalExpr config = do
  baseResult <- parseConditionalExprBase config (parseConditionalExprTokens config)
  let baseExpr = parseResultValue baseResult
      rest = parseResultRemaining baseResult
  case rest of
    Token IfToken _ ifPos : afterIf -> do
      condResult <- parseConditionalExprCondition config afterIf
      let condExpr = parseResultValue condResult
          afterCond = parseResultRemaining condResult
      case afterCond of
        Token ElseToken _ _ : afterElse -> do
          elseResult <- parseConditionalExpr config {parseConditionalExprTokens = afterElse}
          let elseExpr = parseResultValue elseResult
              remaining = parseResultRemaining elseResult
          Right (ParseResult {parseResultValue = ConditionalExpr {conditionalExprThen = baseExpr, conditionalExprCondition = condExpr, conditionalExprElse = elseExpr, conditionalExprPos = ifPos}, parseResultRemaining = remaining})
        _ -> Right baseResult
    _ -> Right baseResult
