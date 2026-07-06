module PythonHS.Parser.ParseYieldStmt (parseYieldStmt) where

import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (FromToken))
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult (..))
import PythonHS.Parser.ParseYieldStmtConfig (ParseYieldStmtConfig (..))

parseYieldStmt :: ParseYieldStmtConfig -> Either ParseError (ParseResult Stmt)
parseYieldStmt config =
  let parseExprFn = parseYieldStmtExpr config
      pos = parseYieldStmtPos config
      tokenStream = parseYieldStmtTokenStream config
   in case tokenStream of
        Token FromToken _ _ : rest -> do
          result <- parseExprFn rest
          let valueExpr = parseResultValue result
              remaining = parseResultRemaining result
          Right (ParseResult {parseResultValue = YieldFromStmt {yieldFromStmtValue = valueExpr, yieldFromStmtPos = pos}, parseResultRemaining = remaining})
        _ -> do
          result <- parseExprFn tokenStream
          let valueExpr = parseResultValue result
              remaining = parseResultRemaining result
          Right (ParseResult {parseResultValue = YieldStmt {yieldStmtValue = valueExpr, yieldStmtPos = pos}, parseResultRemaining = remaining})
