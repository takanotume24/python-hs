module PythonHS.Parser.ParseComprehensionTail (parseComprehensionTail) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType (TokenType (CommaToken, ForToken, IdentifierToken, IfToken, InToken, RBracketToken))
import PythonHS.Parser.ParseComprehensionTailConfig (ParseComprehensionTailConfig (..))
import PythonHS.Parser.ParseError (ParseError (..))

parseComprehensionTail ::
  ParseComprehensionTailConfig ->
  Either ParseError (Expr, [Token])
parseComprehensionTail config =
  let parseExpr = parseComprehensionTailExpr config
      valueExpr = parseComprehensionTailValueExpr config
      listPos = parseComprehensionTailListPos config
      clauses = parseComprehensionTailClauses config
      tokens = parseComprehensionTailTokenStream config
   in case tokens of
        Token IfToken _ _ : rest -> do
          (condExpr, afterCond) <- parseExpr rest
          case reverse clauses of
            [] -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
            (targets, iterExpr, conds) : prevRev ->
              parseComprehensionTail (config {parseComprehensionTailClauses = reverse prevRev ++ [(targets, iterExpr, conds ++ [condExpr])], parseComprehensionTailTokenStream = afterCond})
        Token ForToken _ _ : rest -> do
          (loopTargets, afterIn) <- parseComprehensionTargets rest
          (iterExpr, afterIter) <- parseExpr afterIn
          parseComprehensionTail (config {parseComprehensionTailClauses = clauses ++ [(loopTargets, iterExpr, [])], parseComprehensionTailTokenStream = afterIter})
        Token RBracketToken _ _ : rest ->
          case clauses of
            [([loopVar], iterExpr, [])] -> Right (ListComprehensionExpr {listComprehensionExprValue = valueExpr, listComprehensionExprLoopName = loopVar, listComprehensionExprIter = iterExpr, listComprehensionExprPos = listPos}, rest)
            _ -> Right (ListComprehensionClausesExpr {listComprehensionClausesExprValue = valueExpr, listComprehensionClausesExprClauses = clauses, listComprehensionClausesExprPos = listPos}, rest)
        tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
        _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
  where
    parseComprehensionTargets :: [Token] -> Either ParseError ([String], [Token])
    parseComprehensionTargets (Token IdentifierToken name _ : rest) = parseComprehensionTargetsTail [name] rest
    parseComprehensionTargets (tok : _) = Left (ExpectedExpression {parseErrorPosition = position tok})
    parseComprehensionTargets _ = Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})

    parseComprehensionTargetsTail :: [String] -> [Token] -> Either ParseError ([String], [Token])
    parseComprehensionTargetsTail names (Token InToken _ _ : rest) = Right (names, rest)
    parseComprehensionTargetsTail names (Token CommaToken _ _ : Token IdentifierToken name _ : rest) =
      parseComprehensionTargetsTail (names ++ [name]) rest
    parseComprehensionTargetsTail _ (tok : _) = Left (ExpectedExpression {parseErrorPosition = position tok})
    parseComprehensionTargetsTail _ _ = Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
