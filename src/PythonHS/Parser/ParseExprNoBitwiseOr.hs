module PythonHS.Parser.ParseExprNoBitwiseOr (parseExprNoBitwiseOr) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseExprConfig (ParseExprConfig (..))
import PythonHS.Parser.ParseExprWithConfig (parseExprWithConfig)
import PythonHS.Parser.ParseResult (ParseResult)

parseExprNoBitwiseOr :: [Token] -> Either ParseError (ParseResult Expr)
parseExprNoBitwiseOr tokens = parseExprWithConfig ParseExprConfig {parseExprConfigAllowBitwiseOr = False, parseExprConfigTokens = tokens}
