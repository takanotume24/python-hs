module PythonHS.Parser.ParseComprehensionTail (parseComprehensionTail) where

import PythonHS.AST.Expr (Expr (ListComprehensionClausesExpr, ListComprehensionExpr), Expr)
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (CommaToken, ForToken, IdentifierToken, IfToken, InToken, RBracketToken))
import PythonHS.Parser.ParseComprehensionTailConfig (ParseComprehensionTailConfig (..))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))

parseComprehensionTail ::
  ParseComprehensionTailConfig ->
  [([String], Expr, [Expr])] ->
  [Token] ->
  Either ParseError (Expr, [Token])
parseComprehensionTail config clauses tokens =
  let parseExpr = parseComprehensionTailExpr config
      valueExpr = parseComprehensionTailValueExpr config
      listPos = parseComprehensionTailListPos config
   in case tokens of
        Token IfToken _ _ : rest -> do
          (condExpr, afterCond) <- parseExpr rest
          case reverse clauses of
            [] -> Left (ExpectedExpression (Position 0 0))
            (targets, iterExpr, conds) : prevRev ->
              parseComprehensionTail config (reverse prevRev ++ [(targets, iterExpr, conds ++ [condExpr])]) afterCond
        Token ForToken _ _ : rest -> do
          (loopTargets, afterIn) <- parseComprehensionTargets rest
          (iterExpr, afterIter) <- parseExpr afterIn
          parseComprehensionTail config (clauses ++ [(loopTargets, iterExpr, [])]) afterIter
        Token RBracketToken _ _ : rest ->
          case clauses of
            [([loopVar], iterExpr, [])] -> Right (ListComprehensionExpr valueExpr loopVar iterExpr listPos, rest)
            _ -> Right (ListComprehensionClausesExpr valueExpr clauses listPos, rest)
        tok : _ -> Left (ExpectedExpression (position tok))
        _ -> Left (ExpectedExpression (Position 0 0))
  where
    parseComprehensionTargets :: [Token] -> Either ParseError ([String], [Token])
    parseComprehensionTargets (Token IdentifierToken name _ : rest) = parseComprehensionTargetsTail [name] rest
    parseComprehensionTargets (tok : _) = Left (ExpectedExpression (position tok))
    parseComprehensionTargets _ = Left (ExpectedExpression (Position 0 0))

    parseComprehensionTargetsTail :: [String] -> [Token] -> Either ParseError ([String], [Token])
    parseComprehensionTargetsTail names (Token InToken _ _ : rest) = Right (names, rest)
    parseComprehensionTargetsTail names (Token CommaToken _ _ : Token IdentifierToken name _ : rest) =
      parseComprehensionTargetsTail (names ++ [name]) rest
    parseComprehensionTargetsTail _ (tok : _) = Left (ExpectedExpression (position tok))
    parseComprehensionTargetsTail _ _ = Left (ExpectedExpression (Position 0 0))
