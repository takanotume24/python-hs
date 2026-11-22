module PythonHS.Parser.ParseDelStmt (parseDelStmt) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Parser.ExprPos (exprPos)
import PythonHS.Parser.ParseDelStmtConfig (ParseDelStmtConfig (..))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseResult (ParseResult (..))

parseDelStmt :: ParseDelStmtConfig -> Either ParseError (ParseResult Stmt)
parseDelStmt config =
  case parseExpr tokenStream of
    Right targetResult ->
      case parseResultValue targetResult of
        target@IndexExpr {} ->
          Right
            ParseResult
              { parseResultValue = DeleteSubscriptStmt {deleteSubscriptStmtTarget = target, deleteSubscriptStmtPos = exprPos target},
                parseResultRemaining = parseResultRemaining targetResult
              }
        target@SliceExpr {} ->
          Right
            ParseResult
              { parseResultValue = DeleteSubscriptStmt {deleteSubscriptStmtTarget = target, deleteSubscriptStmtPos = exprPos target},
                parseResultRemaining = parseResultRemaining targetResult
              }
        IdentifierExpr {identifierExprName = name} ->
          Right
            ParseResult
              { parseResultValue = DeleteStmt {deleteStmtTarget = name, deleteStmtPos = pos},
                parseResultRemaining = parseResultRemaining targetResult
              }
        _ -> Left (ExpectedExpression {parseErrorPosition = pos})
    Left err -> Left err
  where
    parseExpr = parseDelStmtExpr config
    pos = parseDelStmtPos config
    tokenStream = parseDelStmtTokenStream config
