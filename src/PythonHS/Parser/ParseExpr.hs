module PythonHS.Parser.ParseExpr (parseExpr) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseExprConfig (ParseExprConfig (..))
import PythonHS.Parser.ParseExprWithConfig (parseExprWithConfig)
import PythonHS.Parser.ParseResult (ParseResult)

parseExpr :: [Token] -> Either ParseError (ParseResult Expr)
parseExpr tokens = parseExprWithConfig ParseExprConfig {parseExprConfigAllowBitwiseOr = True, parseExprConfigTokens = tokens}
