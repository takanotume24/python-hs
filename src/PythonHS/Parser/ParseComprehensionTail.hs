module PythonHS.Parser.ParseComprehensionTail (parseComprehensionTail) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.ListComprehensionClause (ListComprehensionClause (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType (TokenType (CommaToken, ForToken, IdentifierToken, IfToken, InToken, RBracketToken))
import PythonHS.Parser.ParseComprehensionTailConfig (ParseComprehensionTailConfig (..))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseResult (ParseResult (..))

parseComprehensionTail ::
  ParseComprehensionTailConfig ->
  Either ParseError (ParseResult Expr)
parseComprehensionTail config =
  let parseExpr = parseComprehensionTailExpr config
      valueExpr = parseComprehensionTailValueExpr config
      listPos = parseComprehensionTailListPos config
      clauses = parseComprehensionTailClauses config
      tokens = parseComprehensionTailTokenStream config
   in case tokens of
        Token IfToken _ _ : rest -> do
          condResult <- parseExpr rest
          let condExpr = parseResultValue condResult
              afterCond = parseResultRemaining condResult
          case reverse clauses of
            [] -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
            ListComprehensionClause {listComprehensionClauseTargets = targets, listComprehensionClauseIter = iterExpr, listComprehensionClauseConds = conds} : prevRev ->
              parseComprehensionTail (config {parseComprehensionTailClauses = reverse prevRev ++ [ListComprehensionClause {listComprehensionClauseTargets = targets, listComprehensionClauseIter = iterExpr, listComprehensionClauseConds = conds ++ [condExpr]}], parseComprehensionTailTokenStream = afterCond})
        Token ForToken _ _ : rest -> do
          loopTargetsResult <- parseComprehensionTargets rest
          let loopTargets = parseResultValue loopTargetsResult
              afterIn = parseResultRemaining loopTargetsResult
          iterResult <- parseExpr afterIn
          let iterExpr = parseResultValue iterResult
              afterIter = parseResultRemaining iterResult
          parseComprehensionTail (config {parseComprehensionTailClauses = clauses ++ [ListComprehensionClause {listComprehensionClauseTargets = loopTargets, listComprehensionClauseIter = iterExpr, listComprehensionClauseConds = []}], parseComprehensionTailTokenStream = afterIter})
        Token RBracketToken _ _ : rest ->
          case clauses of
            [ListComprehensionClause {listComprehensionClauseTargets = [loopVar], listComprehensionClauseIter = iterExpr, listComprehensionClauseConds = []}] -> Right (ParseResult {parseResultValue = ListComprehensionExpr {listComprehensionExprValue = valueExpr, listComprehensionExprLoopName = loopVar, listComprehensionExprIter = iterExpr, listComprehensionExprPos = listPos}, parseResultRemaining = rest})
            _ -> Right (ParseResult {parseResultValue = ListComprehensionClausesExpr {listComprehensionClausesExprValue = valueExpr, listComprehensionClausesExprClauses = clauses, listComprehensionClausesExprPos = listPos}, parseResultRemaining = rest})
        tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
        _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
  where
    parseComprehensionTargets :: [Token] -> Either ParseError (ParseResult [String])
    parseComprehensionTargets (Token IdentifierToken name _ : rest) = parseComprehensionTargetsTail [name] rest
    parseComprehensionTargets (tok : _) = Left (ExpectedExpression {parseErrorPosition = position tok})
    parseComprehensionTargets _ = Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})

    parseComprehensionTargetsTail :: [String] -> [Token] -> Either ParseError (ParseResult [String])
    parseComprehensionTargetsTail names (Token InToken _ _ : rest) = Right (ParseResult {parseResultValue = names, parseResultRemaining = rest})
    parseComprehensionTargetsTail names (Token CommaToken _ _ : Token IdentifierToken name _ : rest) =
      parseComprehensionTargetsTail (names ++ [name]) rest
    parseComprehensionTargetsTail _ (tok : _) = Left (ExpectedExpression {parseErrorPosition = position tok})
    parseComprehensionTargetsTail _ _ = Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
