module PythonHS.Parser.ParseWalrusExpr (parseWalrusExpr) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (ColonAssignToken, IdentifierToken))
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult (..))
import PythonHS.Parser.ParseWalrusExprConfig (ParseWalrusExprConfig (..))

parseWalrusExpr :: ParseWalrusExprConfig -> Either ParseError (ParseResult Expr)
parseWalrusExpr config =
  let parseFallback = walrusExprFallback config
   in case walrusExprTokens config of
        Token IdentifierToken name pos : Token ColonAssignToken _ _ : rest -> do
          result <- parseFallback rest
          let valueExpr = parseResultValue result
              afterValue = parseResultRemaining result
          Right (ParseResult {parseResultValue = WalrusExpr {walrusExprName = name, walrusExprValue = valueExpr, walrusExprPos = pos}, parseResultRemaining = afterValue})
        tokens -> parseFallback tokens
