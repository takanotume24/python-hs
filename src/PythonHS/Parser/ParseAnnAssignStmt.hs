module PythonHS.Parser.ParseAnnAssignStmt (parseAnnAssignStmt) where

import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (AssignToken))
import PythonHS.Parser.ParseAnnAssignStmtConfig (ParseAnnAssignStmtConfig (..))
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult (..))

parseAnnAssignStmt ::
  ParseAnnAssignStmtConfig ->
  Either ParseError (ParseResult Stmt)
parseAnnAssignStmt config = do
  let parseExpr = parseAnnAssignStmtExpr config
      name = parseAnnAssignStmtName config
      pos = parseAnnAssignStmtPos config
      rest = parseAnnAssignStmtTokenStream config
  annotationResult <- parseExpr rest
  let annotationExpr = parseResultValue annotationResult
      afterAnnotation = parseResultRemaining annotationResult
  case afterAnnotation of
    Token AssignToken _ _ : afterAssign -> do
      valueResult <- parseExpr afterAssign
      let valueExpr = parseResultValue valueResult
          remaining = parseResultRemaining valueResult
      Right (ParseResult {parseResultValue = AnnAssignStmt {annAssignStmtName = name, annAssignStmtAnnotation = annotationExpr, annAssignStmtValue = Just valueExpr, annAssignStmtPos = pos}, parseResultRemaining = remaining})
    _ -> Right (ParseResult {parseResultValue = AnnAssignStmt {annAssignStmtName = name, annAssignStmtAnnotation = annotationExpr, annAssignStmtValue = Nothing, annAssignStmtPos = pos}, parseResultRemaining = afterAnnotation})
