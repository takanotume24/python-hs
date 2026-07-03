module PythonHS.Parser.ParseWalrusExpr
  ( ParseWalrusExprConfig (..),
    parseWalrusExpr,
  )
where

import PythonHS.AST.Expr (Expr (WalrusExpr), Expr)
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (ColonAssignToken, IdentifierToken))
import PythonHS.Parser.ParseError (ParseError)

data ParseWalrusExprConfig = ParseWalrusExprConfig
  { walrusExprFallback :: [Token] -> Either ParseError (Expr, [Token]),
    walrusExprTokens :: [Token]
  }

parseWalrusExpr :: ParseWalrusExprConfig -> Either ParseError (Expr, [Token])
parseWalrusExpr config =
  let parseFallback = walrusExprFallback config
   in case walrusExprTokens config of
        Token IdentifierToken name pos : Token ColonAssignToken _ _ : rest -> do
          (valueExpr, afterValue) <- parseFallback rest
          Right (WalrusExpr name valueExpr pos, afterValue)
        tokens -> parseFallback tokens
