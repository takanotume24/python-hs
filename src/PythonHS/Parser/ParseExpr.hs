module PythonHS.Parser.ParseExpr (parseExpr) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AddOperator, AndOperator, DivideOperator, EqOperator, FloorDivideOperator, GtOperator, GteOperator, LtOperator, LteOperator, ModuloOperator, MultiplyOperator, NotEqOperator, OrOperator, SubtractOperator))
import PythonHS.AST.DictEntry (DictEntry (..))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (AndToken, ColonToken, CommaToken, DotToken, DoubleSlashToken, EqToken, FalseToken, FloatToken, ForToken, GtToken, GteToken, IdentifierToken, IntegerToken, LBraceToken, LBracketToken, LParenToken, LtToken, LteToken, MinusToken, NoneToken, NotEqToken, NotToken, OrToken, PercentToken, PlusToken, RBraceToken, RBracketToken, RParenToken, SlashToken, StarToken, StringToken, TrueToken))
import PythonHS.Parser.ExprPos (exprPos)
import PythonHS.Parser.NormalizeFloatLiteral (normalizeFloatLiteral)
import PythonHS.Parser.ParseCallArgument (parseCallArgument)
import PythonHS.Parser.ParseCallArgumentConfig (ParseCallArgumentConfig (..))
import PythonHS.Parser.ParseCallArgumentResult (ParseCallArgumentResult (..))
import PythonHS.Parser.ParseComprehensionTail (parseComprehensionTail)
import PythonHS.Parser.ParseComprehensionTailConfig (ParseComprehensionTailConfig (..))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseLambdaExpr (parseLambdaExpr)
import PythonHS.Parser.ParseLambdaExprConfig (ParseLambdaExprConfig (..))
import PythonHS.Parser.ParseParenTuple (parseParenTuple)
import PythonHS.Parser.ParseParenTupleConfig (ParseParenTupleConfig (..))
import PythonHS.Parser.ParseResult (ParseResult (..))
import PythonHS.Parser.ParseSubscriptExpr (parseSubscriptExpr)
import PythonHS.Parser.ParseSubscriptExprConfig (ParseSubscriptExprConfig (..))
import PythonHS.Parser.ParseWalrusExpr (parseWalrusExpr)
import PythonHS.Parser.ParseWalrusExprConfig (ParseWalrusExprConfig (..))

parseExpr :: [Token] -> Either ParseError (ParseResult Expr)
parseExpr tokens = parseLambdaExpr (ParseLambdaExprConfig {lambdaExprFallback = \ts -> parseWalrusExpr (ParseWalrusExprConfig {walrusExprFallback = parseOr, walrusExprTokens = ts}), lambdaExprTokens = tokens})
  where
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
      addResult <- parseAdd ts
      let left = parseResultValue addResult
          rest = parseResultRemaining addResult
      parseComparisonTail left rest

    parseComparisonTail left (Token EqToken _ pos : rest) = do
      rightResult <- parseAdd rest
      let right = parseResultValue rightResult
          afterRight = parseResultRemaining rightResult
      parseComparisonTail (BinaryExpr {binaryExprOp = EqOperator, binaryExprLeft = left, binaryExprRight = right, binaryExprPos = pos}) afterRight
    parseComparisonTail left (Token NotEqToken _ pos : rest) = do
      rightResult <- parseAdd rest
      let right = parseResultValue rightResult
          afterRight = parseResultRemaining rightResult
      parseComparisonTail (BinaryExpr {binaryExprOp = NotEqOperator, binaryExprLeft = left, binaryExprRight = right, binaryExprPos = pos}) afterRight
    parseComparisonTail left (Token LtToken _ pos : rest) = do
      rightResult <- parseAdd rest
      let right = parseResultValue rightResult
          afterRight = parseResultRemaining rightResult
      parseComparisonTail (BinaryExpr {binaryExprOp = LtOperator, binaryExprLeft = left, binaryExprRight = right, binaryExprPos = pos}) afterRight
    parseComparisonTail left (Token GtToken _ pos : rest) = do
      rightResult <- parseAdd rest
      let right = parseResultValue rightResult
          afterRight = parseResultRemaining rightResult
      parseComparisonTail (BinaryExpr {binaryExprOp = GtOperator, binaryExprLeft = left, binaryExprRight = right, binaryExprPos = pos}) afterRight
    parseComparisonTail left (Token LteToken _ pos : rest) = do
      rightResult <- parseAdd rest
      let right = parseResultValue rightResult
          afterRight = parseResultRemaining rightResult
      parseComparisonTail (BinaryExpr {binaryExprOp = LteOperator, binaryExprLeft = left, binaryExprRight = right, binaryExprPos = pos}) afterRight
    parseComparisonTail left (Token GteToken _ pos : rest) = do
      rightResult <- parseAdd rest
      let right = parseResultValue rightResult
          afterRight = parseResultRemaining rightResult
      parseComparisonTail (BinaryExpr {binaryExprOp = GteOperator, binaryExprLeft = left, binaryExprRight = right, binaryExprPos = pos}) afterRight
    parseComparisonTail left remaining = Right (ParseResult {parseResultValue = left, parseResultRemaining = remaining})

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
      primaryResult <- parsePrimary tokenStream
      let leftExpr = parseResultValue primaryResult
          remaining = parseResultRemaining primaryResult
      parseMulTail leftExpr remaining

    parseMulTail left (Token StarToken _ pos : rest) = do
      rightResult <- parsePrimary rest
      let rightExpr = parseResultValue rightResult
          remaining = parseResultRemaining rightResult
      parseMulTail (BinaryExpr {binaryExprOp = MultiplyOperator, binaryExprLeft = left, binaryExprRight = rightExpr, binaryExprPos = pos}) remaining
    parseMulTail left (Token SlashToken _ pos : rest) = do
      rightResult <- parsePrimary rest
      let rightExpr = parseResultValue rightResult
          remaining = parseResultRemaining rightResult
      parseMulTail (BinaryExpr {binaryExprOp = DivideOperator, binaryExprLeft = left, binaryExprRight = rightExpr, binaryExprPos = pos}) remaining
    parseMulTail left (Token DoubleSlashToken _ pos : rest) = do
      rightResult <- parsePrimary rest
      let rightExpr = parseResultValue rightResult
          remaining = parseResultRemaining rightResult
      parseMulTail (BinaryExpr {binaryExprOp = FloorDivideOperator, binaryExprLeft = left, binaryExprRight = rightExpr, binaryExprPos = pos}) remaining
    parseMulTail left (Token PercentToken _ pos : rest) = do
      rightResult <- parsePrimary rest
      let rightExpr = parseResultValue rightResult
          remaining = parseResultRemaining rightResult
      parseMulTail (BinaryExpr {binaryExprOp = ModuloOperator, binaryExprLeft = left, binaryExprRight = rightExpr, binaryExprPos = pos}) remaining
    parseMulTail left remaining = Right (ParseResult {parseResultValue = left, parseResultRemaining = remaining})

    parsePrimary tokenStream = do
      atomResult <- parseAtom tokenStream
      let baseExpr = parseResultValue atomResult
          remaining = parseResultRemaining atomResult
      parsePostfix baseExpr remaining

    parseAtom (Token IntegerToken value pos : rest) = Right (ParseResult {parseResultValue = IntegerExpr {integerExprValue = read value, integerExprPos = pos}, parseResultRemaining = rest})
    parseAtom (Token FloatToken value pos : rest) = Right (ParseResult {parseResultValue = FloatExpr {floatExprValue = read (normalizeFloatLiteral value), floatExprPos = pos}, parseResultRemaining = rest})
    parseAtom (Token TrueToken _ pos : rest) = Right (ParseResult {parseResultValue = IntegerExpr {integerExprValue = 1, integerExprPos = pos}, parseResultRemaining = rest})
    parseAtom (Token FalseToken _ pos : rest) = Right (ParseResult {parseResultValue = IntegerExpr {integerExprValue = 0, integerExprPos = pos}, parseResultRemaining = rest})
    parseAtom (Token NoneToken _ pos : rest) = Right (ParseResult {parseResultValue = NoneExpr {noneExprPos = pos}, parseResultRemaining = rest})
    parseAtom (Token MinusToken _ pos : Token IntegerToken value _ : rest) = Right (ParseResult {parseResultValue = IntegerExpr {integerExprValue = negate (read value), integerExprPos = pos}, parseResultRemaining = rest})
    parseAtom (Token MinusToken _ pos : Token FloatToken value _ : rest) = Right (ParseResult {parseResultValue = FloatExpr {floatExprValue = negate (read (normalizeFloatLiteral value)), floatExprPos = pos}, parseResultRemaining = rest})
    parseAtom (Token MinusToken _ pos : rest) = do
      primaryResult <- parsePrimary rest
      let expr = parseResultValue primaryResult
          remaining = parseResultRemaining primaryResult
      Right (ParseResult {parseResultValue = UnaryMinusExpr {unaryMinusExprValue = expr, unaryMinusExprPos = pos}, parseResultRemaining = remaining})
    parseAtom (Token StringToken value pos : rest) = Right (ParseResult {parseResultValue = StringExpr {stringExprValue = value, stringExprPos = pos}, parseResultRemaining = rest})
    parseAtom (Token LBracketToken _ pos : rest) = parseListElements pos rest
    parseAtom (Token LBraceToken _ pos : rest) = parseDictEntries pos rest
    parseAtom (Token LParenToken _ parenPos : rest) =
      parseParenTuple (ParseParenTupleConfig {parseParenTupleExpr = parseExpr, parseParenTuplePos = parenPos, parseParenTupleTokenStream = rest})
    parseAtom (Token IdentifierToken value pos : rest) = Right (ParseResult {parseResultValue = IdentifierExpr {identifierExprName = value, identifierExprPos = pos}, parseResultRemaining = rest})
    parseAtom (tok : _) = Left (ExpectedExpression {parseErrorPosition = position tok})
    parseAtom _ = Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})

    parsePostfix (IdentifierExpr name pos) (Token LParenToken _ _ : rest) = do
      argsResult <- parseArguments rest
      let args = parseResultValue argsResult
          afterArgs = parseResultRemaining argsResult
      parsePostfix (CallExpr {callExprName = name, callExprArgs = args, callExprPos = pos}) afterArgs
    parsePostfix calleeExpr (Token LParenToken _ _ : rest) = do
      argsResult <- parseArguments rest
      let args = parseResultValue argsResult
          afterArgs = parseResultRemaining argsResult
      parsePostfix (CallValueExpr {callValueExprCallee = calleeExpr, callValueExprArgs = args, callValueExprPos = exprPos calleeExpr}) afterArgs
    parsePostfix receiverExpr (Token DotToken _ _ : Token IdentifierToken methodName methodPos : Token LParenToken _ _ : rest) = do
      argsResult <- parseArguments rest
      let args = parseResultValue argsResult
          afterArgs = parseResultRemaining argsResult
      parsePostfix (CallExpr {callExprName = methodName, callExprArgs = receiverExpr : args, callExprPos = methodPos}) afterArgs
    parsePostfix receiverExpr (Token LBracketToken _ pos : rest) = do
      subResult <- parseSubscriptExpr (ParseSubscriptExprConfig {parseSubscriptExprFn = parseExpr, parseSubscriptExprReceiver = receiverExpr, parseSubscriptExprPos = pos, parseSubscriptExprTokenStream = rest})
      let subscriptExpr = parseResultValue subResult
          afterSubscript = parseResultRemaining subResult
      parsePostfix subscriptExpr afterSubscript
    parsePostfix (IdentifierExpr receiverName receiverPos) (Token DotToken _ _ : Token IdentifierToken attrName _ : rest) =
      parsePostfix (IdentifierExpr {identifierExprName = receiverName ++ "." ++ attrName, identifierExprPos = receiverPos}) rest
    parsePostfix expr rest = Right (ParseResult {parseResultValue = expr, parseResultRemaining = rest})

    parseListElements listPos (Token RBracketToken _ _ : rest) =
      Right (ParseResult {parseResultValue = ListExpr {listExprItems = [], listExprPos = listPos}, parseResultRemaining = rest})
    parseListElements listPos ts = do
      firstResult <- parseExpr ts
      let firstExpr = parseResultValue firstResult
          afterFirst = parseResultRemaining firstResult
      case afterFirst of
        forTokens@(Token ForToken _ _ : _) ->
          parseComprehensionTail (ParseComprehensionTailConfig {parseComprehensionTailExpr = parseExpr, parseComprehensionTailValueExpr = firstExpr, parseComprehensionTailListPos = listPos, parseComprehensionTailClauses = [], parseComprehensionTailTokenStream = forTokens})
        _ -> parseListTail listPos [firstExpr] afterFirst

    parseListTail listPos exprs (Token CommaToken _ _ : rest) = do
      case rest of
        Token RBracketToken _ _ : afterBracket -> Right (ParseResult {parseResultValue = ListExpr {listExprItems = exprs, listExprPos = listPos}, parseResultRemaining = afterBracket})
        _ -> do
          nextResult <- parseExpr rest
          let nextExpr = parseResultValue nextResult
              afterNext = parseResultRemaining nextResult
          parseListTail listPos (exprs ++ [nextExpr]) afterNext
    parseListTail listPos exprs (Token RBracketToken _ _ : rest) =
      Right (ParseResult {parseResultValue = ListExpr {listExprItems = exprs, listExprPos = listPos}, parseResultRemaining = rest})
    parseListTail _ _ (tok : _) = Left (ExpectedExpression {parseErrorPosition = position tok})
    parseListTail _ _ _ = Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})

    parseDictEntries dictPos (Token RBraceToken _ _ : rest) =
      Right (ParseResult {parseResultValue = DictExpr {dictExprEntries = [], dictExprPos = dictPos}, parseResultRemaining = rest})
    parseDictEntries dictPos ts = do
      keyResult <- parseExpr ts
      let keyExpr = parseResultValue keyResult
          afterKey = parseResultRemaining keyResult
      case afterKey of
        Token ColonToken _ _ : afterColon -> do
          valueResult <- parseExpr afterColon
          let valueExpr = parseResultValue valueResult
              afterValue = parseResultRemaining valueResult
          parseDictTail dictPos [DictEntry {dictEntryKey = keyExpr, dictEntryValue = valueExpr}] afterValue
        tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
        _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})

    parseDictTail dictPos pairs (Token CommaToken _ _ : rest) = do
      case rest of
        Token RBraceToken _ _ : afterBrace -> Right (ParseResult {parseResultValue = DictExpr {dictExprEntries = pairs, dictExprPos = dictPos}, parseResultRemaining = afterBrace})
        _ -> do
          nextKeyResult <- parseExpr rest
          let nextKey = parseResultValue nextKeyResult
              afterKey = parseResultRemaining nextKeyResult
          case afterKey of
            Token ColonToken _ _ : afterColon -> do
              nextValueResult <- parseExpr afterColon
              let nextValue = parseResultValue nextValueResult
                  afterValue = parseResultRemaining nextValueResult
              parseDictTail dictPos (pairs ++ [DictEntry {dictEntryKey = nextKey, dictEntryValue = nextValue}]) afterValue
            tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
    parseDictTail dictPos pairs (Token RBraceToken _ _ : rest) =
      Right (ParseResult {parseResultValue = DictExpr {dictExprEntries = pairs, dictExprPos = dictPos}, parseResultRemaining = rest})
    parseDictTail _ _ (tok : _) = Left (ExpectedExpression {parseErrorPosition = position tok})
    parseDictTail _ _ _ = Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})

    parseArguments (Token RParenToken _ _ : rest) = Right (ParseResult {parseResultValue = [], parseResultRemaining = rest})
    parseArguments ts = parseArgumentsTail False [] ts
    parseArgumentsTail seenKeywordArg accArgs tokenStream = do
      argResult <- parseCallArgument (ParseCallArgumentConfig {parseCallArgumentExpr = parseExpr, parseCallArgumentTokenStream = tokenStream})
      let argExpr = parseCallArgumentResultExpr (parseResultValue argResult)
          isKeywordArg = parseCallArgumentResultIsKeyword (parseResultValue argResult)
          mismatchPos = parseCallArgumentResultPos (parseResultValue argResult)
          afterArg = parseResultRemaining argResult
      if seenKeywordArg && not isKeywordArg
        then Left (ExpectedExpression {parseErrorPosition = mismatchPos})
        else case afterArg of
          Token RParenToken _ _ : rest -> Right (ParseResult {parseResultValue = accArgs ++ [argExpr], parseResultRemaining = rest})
          Token CommaToken _ _ : rest ->
            case rest of
              Token RParenToken _ _ : afterParen -> Right (ParseResult {parseResultValue = accArgs ++ [argExpr], parseResultRemaining = afterParen})
              _ -> parseArgumentsTail (seenKeywordArg || isKeywordArg) (accArgs ++ [argExpr]) rest
          Token _ _ pos : _ -> Left (ExpectedExpression {parseErrorPosition = pos})
          _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
