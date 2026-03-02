module PythonHS.Parser.ParseAnnAssignStmt (parseAnnAssignStmt) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt (AnnAssignStmt))
import PythonHS.Lexer.Position (Position)
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (AssignToken))
import PythonHS.Parser.ParseError (ParseError)

parseAnnAssignStmt ::
  ([Token] -> Either ParseError (Expr, [Token])) ->
  String ->
  Position ->
  [Token] ->
  Either ParseError (Stmt, [Token])
parseAnnAssignStmt parseExpr name pos rest = do
  (annotationExpr, afterAnnotation) <- parseExpr rest
  case afterAnnotation of
    Token AssignToken _ _ : afterAssign -> do
      (valueExpr, remaining) <- parseExpr afterAssign
      Right (AnnAssignStmt name annotationExpr (Just valueExpr) pos, remaining)
    _ -> Right (AnnAssignStmt name annotationExpr Nothing pos, afterAnnotation)
