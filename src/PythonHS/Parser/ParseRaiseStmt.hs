module PythonHS.Parser.ParseRaiseStmt (parseRaiseStmt) where

import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType (TokenType (FromToken, NewlineToken))
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseRaiseStmtConfig (ParseRaiseStmtConfig (..))
import PythonHS.Parser.ParseResult (ParseResult (..))

parseRaiseStmt :: ParseRaiseStmtConfig -> Either ParseError (ParseResult Stmt)
parseRaiseStmt config =
  let pos = parseRaiseStmtPos config
      rest = parseRaiseStmtTokenStream config
      parseExpr' = parseRaiseStmtExpr config
   in case rest of
        (Token {tokenType = NewlineToken} : _) ->
          Right (ParseResult {parseResultValue = RaiseStmt {raiseStmtExpr = Nothing, raiseStmtPos = pos}, parseResultRemaining = rest})
        _ -> do
          valueResult <- parseExpr' rest
          let valueExpr = parseResultValue valueResult
              afterValue = parseResultRemaining valueResult
          remaining <-
            case afterValue of
              Token {tokenType = FromToken} : afterFrom -> do
                causeResult <- parseExpr' afterFrom
                Right (parseResultRemaining causeResult)
              _ -> Right afterValue
          Right (ParseResult {parseResultValue = RaiseStmt {raiseStmtExpr = Just valueExpr, raiseStmtPos = pos}, parseResultRemaining = remaining})
