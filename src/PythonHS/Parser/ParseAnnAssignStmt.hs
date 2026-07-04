module PythonHS.Parser.ParseAnnAssignStmt (parseAnnAssignStmt) where

import PythonHS.AST.Stmt (Stmt (..))
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
      Right (AnnAssignStmt {annAssignStmtName = name, annAssignStmtAnnotation = annotationExpr, annAssignStmtValue = Just valueExpr, annAssignStmtPos = pos}, remaining)
    _ -> Right (AnnAssignStmt {annAssignStmtName = name, annAssignStmtAnnotation = annotationExpr, annAssignStmtValue = Nothing, annAssignStmtPos = pos}, afterAnnotation)
