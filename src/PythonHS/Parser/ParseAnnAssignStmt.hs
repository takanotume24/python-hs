module PythonHS.Parser.ParseAnnAssignStmt (parseAnnAssignStmt) where

import PythonHS.AST.Stmt (Stmt (AnnAssignStmt))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (AssignToken))
import PythonHS.Parser.ParseAnnAssignStmtConfig (ParseAnnAssignStmtConfig (..))
import PythonHS.Parser.ParseError (ParseError)

parseAnnAssignStmt ::
  ParseAnnAssignStmtConfig ->
  [Token] ->
  Either ParseError (Stmt, [Token])
parseAnnAssignStmt config rest = do
  let parseExpr = parseAnnAssignStmtExpr config
      name = parseAnnAssignStmtName config
      pos = parseAnnAssignStmtPos config
  (annotationExpr, afterAnnotation) <- parseExpr rest
  case afterAnnotation of
    Token AssignToken _ _ : afterAssign -> do
      (valueExpr, remaining) <- parseExpr afterAssign
      Right (AnnAssignStmt name annotationExpr (Just valueExpr) pos, remaining)
    _ -> Right (AnnAssignStmt name annotationExpr Nothing pos, afterAnnotation)
