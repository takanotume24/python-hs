module PythonHS.Parser.ParsePowerExpr (parsePowerExpr) where

import PythonHS.AST.BinaryOperator (BinaryOperator (PowerOperator))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType (TokenType (DoubleStarToken, FloatToken, IntegerToken, MinusToken, TildeToken))
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParsePowerExprConfig (ParsePowerExprConfig (..))
import PythonHS.Parser.ParseResult (ParseResult (..))

-- | Parse exponentiation together with its unary prefixes.
--
-- Unary operators bind less tightly than exponentiation in Python. They are
-- handled before the primary is parsed so that @-2 ** 3@ becomes
-- @-(2 ** 3)@ while @(-2) ** 3@ keeps its parenthesised meaning.
parsePowerExpr :: ParsePowerExprConfig -> Either ParseError (ParseResult Expr)
parsePowerExpr config =
  case tokens of
    Token MinusToken _ pos : Token IntegerToken value _ : rest
      | not (startsWithPower rest) ->
          Right (ParseResult {parseResultValue = IntegerExpr {integerExprValue = negate (read value), integerExprPos = pos}, parseResultRemaining = rest})
    Token MinusToken _ pos : Token FloatToken value _ : rest
      | not (startsWithPower rest) ->
          Right (ParseResult {parseResultValue = FloatExpr {floatExprValue = negate (read value), floatExprPos = pos}, parseResultRemaining = rest})
    Token MinusToken _ pos : rest -> wrapUnary (wrapMinus pos) rest
    Token TildeToken _ pos : rest -> wrapUnary (wrapNot pos) rest
    _ -> parsePowerAtom tokens
  where
    tokens = parsePowerExprConfigTokens config
    parsePrimaryFn = parsePowerExprConfigParsePrimary config

    parsePower tokenStream = parsePowerExpr config {parsePowerExprConfigTokens = tokenStream}

    startsWithPower (Token DoubleStarToken _ _ : _) = True
    startsWithPower _ = False

    wrapUnary wrap rest = do
      operandResult <- parsePower rest
      Right (ParseResult {parseResultValue = wrap (parseResultValue operandResult), parseResultRemaining = parseResultRemaining operandResult})

    wrapMinus pos inner = UnaryMinusExpr {unaryMinusExprValue = inner, unaryMinusExprPos = pos}

    wrapNot pos inner = BitwiseNotExpr {bitwiseNotExprValue = inner, bitwiseNotExprPos = pos}

    parsePowerAtom tokenStream = do
      primaryResult <- parsePrimaryFn tokenStream
      parsePowerTail (parseResultValue primaryResult) (parseResultRemaining primaryResult)

    parsePowerTail left (Token DoubleStarToken _ pos : rest) = do
      rightResult <- parsePower rest
      let rightExpr = parseResultValue rightResult
          remaining = parseResultRemaining rightResult
      Right (ParseResult {parseResultValue = BinaryExpr {binaryExprOp = PowerOperator, binaryExprLeft = left, binaryExprRight = rightExpr, binaryExprPos = pos}, parseResultRemaining = remaining})
    parsePowerTail left remaining = Right (ParseResult {parseResultValue = left, parseResultRemaining = remaining})
