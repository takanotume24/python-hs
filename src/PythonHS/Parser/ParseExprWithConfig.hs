module PythonHS.Parser.ParseExprWithConfig (parseExprWithConfig) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AddOperator, AndOperator, DivideOperator, EqOperator, FloorDivideOperator, GtOperator, GteOperator, InOperator, IsNotOperator, IsOperator, LtOperator, LteOperator, ModuloOperator, MultiplyOperator, NotEqOperator, NotInOperator, OrOperator, SubtractOperator))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType
  ( TokenType
      ( AndToken,
        DoubleSlashToken,
        EqToken,
        GtToken,
        GteToken,
        InToken,
        IsToken,
        LtToken,
        LteToken,
        MinusToken,
        NotEqToken,
        NotToken,
        OrToken,
        PercentToken,
        PlusToken,
        SlashToken,
        StarToken
      ),
  )
import PythonHS.Parser.ExprPos (exprPos)
import PythonHS.Parser.ParseBitwiseExpr (parseBitwiseExpr)
import PythonHS.Parser.ParseBitwiseExprConfig (ParseBitwiseExprConfig (..))
import PythonHS.Parser.ParseComparisonItem (ParseComparisonItem (..))
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseExprConfig (ParseExprConfig (..))
import PythonHS.Parser.ParseLambdaExpr (parseLambdaExpr)
import PythonHS.Parser.ParseLambdaExprConfig (ParseLambdaExprConfig (..))
import PythonHS.Parser.ParsePowerExpr (parsePowerExpr)
import PythonHS.Parser.ParsePowerExprConfig (ParsePowerExprConfig (..))
import PythonHS.Parser.ParsePrimary (parsePrimary)
import PythonHS.Parser.ParsePrimaryConfig (ParsePrimaryConfig (..))
import PythonHS.Parser.ParseResult (ParseResult (..))
import PythonHS.Parser.ParseWalrusExpr (parseWalrusExpr)
import PythonHS.Parser.ParseWalrusExprConfig (ParseWalrusExprConfig (..))

parseExprWithConfig :: ParseExprConfig -> Either ParseError (ParseResult Expr)
parseExprWithConfig config = parseLambdaExpr (ParseLambdaExprConfig {lambdaExprFallback = \ts -> parseWalrusExpr (ParseWalrusExprConfig {walrusExprFallback = parseOr, walrusExprTokens = ts}), lambdaExprTokens = tokens})
  where
    allowBitwiseOr = parseExprConfigAllowBitwiseOr config
    tokens = parseExprConfigTokens config
    parseOr ts = do
      andResult <- parseAnd ts
      let left = parseResultValue andResult
          rest = parseResultRemaining andResult
      parseOrTail left rest

    parseOrTail left (Token OrToken _ pos : rest) = do
      rightResult <- parseAnd rest
      let right = parseResultValue rightResult
          afterRight = parseResultRemaining rightResult
      parseOrTail (BinaryExpr {binaryExprOp = OrOperator, binaryExprLeft = left, binaryExprRight = right, binaryExprPos = pos}) afterRight
    parseOrTail left rest = Right (ParseResult {parseResultValue = left, parseResultRemaining = rest})

    parseAnd ts = do
      notResult <- parseNot ts
      let left = parseResultValue notResult
          rest = parseResultRemaining notResult
      parseAndTail left rest

    parseAndTail left (Token AndToken _ pos : rest) = do
      rightResult <- parseNot rest
      let right = parseResultValue rightResult
          afterRight = parseResultRemaining rightResult
      parseAndTail (BinaryExpr {binaryExprOp = AndOperator, binaryExprLeft = left, binaryExprRight = right, binaryExprPos = pos}) afterRight
    parseAndTail left rest = Right (ParseResult {parseResultValue = left, parseResultRemaining = rest})

    parseNot (Token NotToken _ pos : rest) = do
      notResult <- parseNot rest
      let expr = parseResultValue notResult
          remaining = parseResultRemaining notResult
      Right (ParseResult {parseResultValue = NotExpr {notExprValue = expr, notExprPos = pos}, parseResultRemaining = remaining})
    parseNot ts = parseComparison ts

    parseComparison ts = do
      nextResult <- parseComparisonOperand ts
      let left = parseResultValue nextResult
          rest = parseResultRemaining nextResult
      itemsResult <- parseComparisonTail [] rest
      let items = parseResultValue itemsResult
          remaining = parseResultRemaining itemsResult
      case items of
        [] -> Right (ParseResult {parseResultValue = left, parseResultRemaining = remaining})
        [ParseComparisonItem {parseComparisonItemOp = op, parseComparisonItemExpr = right, parseComparisonItemPos = pos}] -> Right (ParseResult {parseResultValue = BinaryExpr {binaryExprOp = op, binaryExprLeft = left, binaryExprRight = right, binaryExprPos = pos}, parseResultRemaining = remaining})
        _ ->
          let firstPos = case items of [] -> exprPos left; firstItem : _ -> parseComparisonItemPos firstItem
           in Right (ParseResult {parseResultValue = ComparisonChainExpr {comparisonChainLeft = left, comparisonChainOps = map parseComparisonItemOp items, comparisonChainRights = map parseComparisonItemExpr items, comparisonChainPos = firstPos}, parseResultRemaining = remaining})

    parseComparisonOperand ts = if allowBitwiseOr then parseBitwiseExpr ParseBitwiseExprConfig {parseBitwiseExprConfigNextParser = parseAdd, parseBitwiseExprConfigTokens = ts} else parseAdd ts

    parseComparisonTail acc (Token EqToken _ pos : rest) = do
      rightResult <- parseComparisonOperand rest
      parseComparisonTail (ParseComparisonItem {parseComparisonItemOp = EqOperator, parseComparisonItemExpr = parseResultValue rightResult, parseComparisonItemPos = pos} : acc) (parseResultRemaining rightResult)
    parseComparisonTail acc (Token NotEqToken _ pos : rest) = do
      rightResult <- parseComparisonOperand rest
      parseComparisonTail (ParseComparisonItem {parseComparisonItemOp = NotEqOperator, parseComparisonItemExpr = parseResultValue rightResult, parseComparisonItemPos = pos} : acc) (parseResultRemaining rightResult)
    parseComparisonTail acc (Token LtToken _ pos : rest) = do
      rightResult <- parseComparisonOperand rest
      parseComparisonTail (ParseComparisonItem {parseComparisonItemOp = LtOperator, parseComparisonItemExpr = parseResultValue rightResult, parseComparisonItemPos = pos} : acc) (parseResultRemaining rightResult)
    parseComparisonTail acc (Token GtToken _ pos : rest) = do
      rightResult <- parseComparisonOperand rest
      parseComparisonTail (ParseComparisonItem {parseComparisonItemOp = GtOperator, parseComparisonItemExpr = parseResultValue rightResult, parseComparisonItemPos = pos} : acc) (parseResultRemaining rightResult)
    parseComparisonTail acc (Token LteToken _ pos : rest) = do
      rightResult <- parseComparisonOperand rest
      parseComparisonTail (ParseComparisonItem {parseComparisonItemOp = LteOperator, parseComparisonItemExpr = parseResultValue rightResult, parseComparisonItemPos = pos} : acc) (parseResultRemaining rightResult)
    parseComparisonTail acc (Token GteToken _ pos : rest) = do
      rightResult <- parseComparisonOperand rest
      parseComparisonTail (ParseComparisonItem {parseComparisonItemOp = GteOperator, parseComparisonItemExpr = parseResultValue rightResult, parseComparisonItemPos = pos} : acc) (parseResultRemaining rightResult)
    parseComparisonTail acc (Token InToken _ pos : rest) = do
      rightResult <- parseComparisonOperand rest
      parseComparisonTail (ParseComparisonItem {parseComparisonItemOp = InOperator, parseComparisonItemExpr = parseResultValue rightResult, parseComparisonItemPos = pos} : acc) (parseResultRemaining rightResult)
    parseComparisonTail acc (Token NotToken _ pos : Token InToken _ _ : rest) = do
      rightResult <- parseComparisonOperand rest
      parseComparisonTail (ParseComparisonItem {parseComparisonItemOp = NotInOperator, parseComparisonItemExpr = parseResultValue rightResult, parseComparisonItemPos = pos} : acc) (parseResultRemaining rightResult)
    parseComparisonTail acc (Token IsToken _ pos : Token NotToken _ _ : rest) = do
      rightResult <- parseComparisonOperand rest
      parseComparisonTail (ParseComparisonItem {parseComparisonItemOp = IsNotOperator, parseComparisonItemExpr = parseResultValue rightResult, parseComparisonItemPos = pos} : acc) (parseResultRemaining rightResult)
    parseComparisonTail acc (Token IsToken _ pos : rest) = do
      rightResult <- parseComparisonOperand rest
      parseComparisonTail (ParseComparisonItem {parseComparisonItemOp = IsOperator, parseComparisonItemExpr = parseResultValue rightResult, parseComparisonItemPos = pos} : acc) (parseResultRemaining rightResult)
    parseComparisonTail acc remaining = Right (ParseResult {parseResultValue = reverse acc, parseResultRemaining = remaining})

    parseAdd tokenStream = do
      mulResult <- parseMul tokenStream
      let leftExpr = parseResultValue mulResult
          remaining = parseResultRemaining mulResult
      parseAddTail leftExpr remaining

    parseAddTail left (Token PlusToken _ pos : rest) = do
      rightResult <- parseMul rest
      let rightExpr = parseResultValue rightResult
          remaining = parseResultRemaining rightResult
      parseAddTail (BinaryExpr {binaryExprOp = AddOperator, binaryExprLeft = left, binaryExprRight = rightExpr, binaryExprPos = pos}) remaining
    parseAddTail left (Token MinusToken _ pos : rest) = do
      rightResult <- parseMul rest
      let rightExpr = parseResultValue rightResult
          remaining = parseResultRemaining rightResult
      parseAddTail (BinaryExpr {binaryExprOp = SubtractOperator, binaryExprLeft = left, binaryExprRight = rightExpr, binaryExprPos = pos}) remaining
    parseAddTail left remaining = Right (ParseResult {parseResultValue = left, parseResultRemaining = remaining})

    parseMul tokenStream = do
      powerResult <- parsePower tokenStream
      let leftExpr = parseResultValue powerResult
          remaining = parseResultRemaining powerResult
      parseMulTail leftExpr remaining

    parseMulTail left (Token StarToken _ pos : rest) = do
      rightResult <- parsePower rest
      let rightExpr = parseResultValue rightResult
          remaining = parseResultRemaining rightResult
      parseMulTail (BinaryExpr {binaryExprOp = MultiplyOperator, binaryExprLeft = left, binaryExprRight = rightExpr, binaryExprPos = pos}) remaining
    parseMulTail left (Token SlashToken _ pos : rest) = do
      rightResult <- parsePower rest
      let rightExpr = parseResultValue rightResult
          remaining = parseResultRemaining rightResult
      parseMulTail (BinaryExpr {binaryExprOp = DivideOperator, binaryExprLeft = left, binaryExprRight = rightExpr, binaryExprPos = pos}) remaining
    parseMulTail left (Token DoubleSlashToken _ pos : rest) = do
      rightResult <- parsePower rest
      let rightExpr = parseResultValue rightResult
          remaining = parseResultRemaining rightResult
      parseMulTail (BinaryExpr {binaryExprOp = FloorDivideOperator, binaryExprLeft = left, binaryExprRight = rightExpr, binaryExprPos = pos}) remaining
    parseMulTail left (Token PercentToken _ pos : rest) = do
      rightResult <- parsePower rest
      let rightExpr = parseResultValue rightResult
          remaining = parseResultRemaining rightResult
      parseMulTail (BinaryExpr {binaryExprOp = ModuloOperator, binaryExprLeft = left, binaryExprRight = rightExpr, binaryExprPos = pos}) remaining
    parseMulTail left remaining = Right (ParseResult {parseResultValue = left, parseResultRemaining = remaining})

    parsePower tokenStream =
      parsePowerExpr
        ParsePowerExprConfig
          { parsePowerExprConfigParsePrimary = \ts ->
              parsePrimary
                ParsePrimaryConfig
                  { parsePrimaryExpr = \nestedTokens ->
                      parseExprWithConfig
                        ParseExprConfig
                          { parseExprConfigAllowBitwiseOr = allowBitwiseOr,
                            parseExprConfigTokens = nestedTokens
                          },
                    parsePrimaryTokenStream = ts
                  },
            parsePowerExprConfigTokens = tokenStream
          }
