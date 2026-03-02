module PythonHS.Parser.ParseComprehensionTail (parseComprehensionTail) where

import PythonHS.AST.Expr (Expr (ListComprehensionClausesExpr, ListComprehensionExpr), Expr)
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (ForToken, IdentifierToken, IfToken, InToken, RBracketToken))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))

parseComprehensionTail ::
  ([Token] -> Either ParseError (Expr, [Token])) ->
  (Expr -> Position) ->
  Expr ->
  Position ->
  [(String, Expr, Maybe Expr)] ->
  [Token] ->
  Either ParseError (Expr, [Token])
parseComprehensionTail parseExpr exprPos valueExpr listPos clauses (Token IfToken _ _ : rest) = do
  (condExpr, afterCond) <- parseExpr rest
  case reverse clauses of
    [] -> Left (ExpectedExpression (Position 0 0))
    (name, iterExpr, Nothing) : prevRev ->
      parseComprehensionTail parseExpr exprPos valueExpr listPos (reverse prevRev ++ [(name, iterExpr, Just condExpr)]) afterCond
    (_ : _) -> Left (ExpectedExpression (exprPos condExpr))
parseComprehensionTail parseExpr exprPos valueExpr listPos clauses (Token ForToken _ _ : Token IdentifierToken loopVar _ : Token InToken _ _ : afterIn) = do
  (iterExpr, afterIter) <- parseExpr afterIn
  parseComprehensionTail parseExpr exprPos valueExpr listPos (clauses ++ [(loopVar, iterExpr, Nothing)]) afterIter
parseComprehensionTail _ _ valueExpr listPos clauses (Token RBracketToken _ _ : rest) =
  case clauses of
    [(loopVar, iterExpr, Nothing)] -> Right (ListComprehensionExpr valueExpr loopVar iterExpr listPos, rest)
    _ -> Right (ListComprehensionClausesExpr valueExpr clauses listPos, rest)
parseComprehensionTail _ _ _ _ _ (tok : _) = Left (ExpectedExpression (position tok))
parseComprehensionTail _ _ _ _ _ _ = Left (ExpectedExpression (Position 0 0))
