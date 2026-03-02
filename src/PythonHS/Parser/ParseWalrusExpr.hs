module PythonHS.Parser.ParseWalrusExpr (parseWalrusExpr) where

import PythonHS.AST.Expr (Expr (WalrusExpr), Expr)
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (ColonAssignToken, IdentifierToken))
import PythonHS.Parser.ParseError (ParseError)

parseWalrusExpr :: ([Token] -> Either ParseError (Expr, [Token])) -> [Token] -> Either ParseError (Expr, [Token])
parseWalrusExpr parseFallback (Token IdentifierToken name pos : Token ColonAssignToken _ _ : rest) = do
  (valueExpr, afterValue) <- parseFallback rest
  Right (WalrusExpr name valueExpr pos, afterValue)
parseWalrusExpr parseFallback tokens = parseFallback tokens
