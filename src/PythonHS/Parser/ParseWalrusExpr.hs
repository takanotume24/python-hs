module PythonHS.Parser.ParseWalrusExpr (parseWalrusExpr) where

import PythonHS.AST.Expr (Expr (WalrusExpr))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (ColonAssignToken, IdentifierToken))
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseWalrusExprConfig (ParseWalrusExprConfig (..))

parseWalrusExpr :: ParseWalrusExprConfig -> Either ParseError (Expr, [Token])
parseWalrusExpr config =
  let parseFallback = walrusExprFallback config
   in case walrusExprTokens config of
        Token IdentifierToken name pos : Token ColonAssignToken _ _ : rest -> do
          (valueExpr, afterValue) <- parseFallback rest
          Right (WalrusExpr name valueExpr pos, afterValue)
        tokens -> parseFallback tokens
