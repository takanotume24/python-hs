module PythonHS.Parser.ParseExpr (parseExpr) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AddOperator, AndOperator, DivideOperator, EqOperator, FloorDivideOperator, GtOperator, GteOperator, LtOperator, LteOperator, ModuloOperator, MultiplyOperator, NotEqOperator, OrOperator, SubtractOperator))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (AndToken, ColonToken, CommaToken, DotToken, DoubleSlashToken, EqToken, FalseToken, FloatToken, ForToken, GtToken, GteToken, IdentifierToken, IntegerToken, LBraceToken, LBracketToken, LParenToken, LtToken, LteToken, MinusToken, NoneToken, NotEqToken, NotToken, OrToken, PercentToken, PlusToken, RBraceToken, RBracketToken, RParenToken, SlashToken, StarToken, StringToken, TrueToken))
import PythonHS.Parser.ExprPos (exprPos)
import PythonHS.Parser.NormalizeFloatLiteral (normalizeFloatLiteral)
import PythonHS.Parser.ParseCallArgument (parseCallArgument)
import PythonHS.Parser.ParseCallArgumentConfig (ParseCallArgumentConfig (..))
import PythonHS.Parser.ParseComprehensionTail (parseComprehensionTail)
import PythonHS.Parser.ParseComprehensionTailConfig (ParseComprehensionTailConfig (..))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseLambdaExpr (parseLambdaExpr)
import PythonHS.Parser.ParseLambdaExprConfig (ParseLambdaExprConfig (..))
import PythonHS.Parser.ParseParenTuple (parseParenTuple)
import PythonHS.Parser.ParseParenTupleConfig (ParseParenTupleConfig (..))
import PythonHS.Parser.ParseSubscriptExpr (parseSubscriptExpr)
import PythonHS.Parser.ParseSubscriptExprConfig (ParseSubscriptExprConfig (..))
import PythonHS.Parser.ParseWalrusExpr (parseWalrusExpr)
import PythonHS.Parser.ParseWalrusExprConfig (ParseWalrusExprConfig (..))

parseExpr :: [Token] -> Either ParseError (Expr, [Token])
parseExpr tokens = parseLambdaExpr (ParseLambdaExprConfig {lambdaExprFallback = \ts -> parseWalrusExpr (ParseWalrusExprConfig {walrusExprFallback = parseOr, walrusExprTokens = ts}), lambdaExprTokens = tokens})
  where
    parseOr ts = do
      (left, rest) <- parseAnd ts
      parseOrTail left rest

    parseOrTail left (Token OrToken _ pos : rest) = do
      (right, afterRight) <- parseAnd rest
      parseOrTail (BinaryExpr {binaryExprOp = OrOperator, binaryExprLeft = left, binaryExprRight = right, binaryExprPos = pos}) afterRight
    parseOrTail left rest = Right (left, rest)

    parseAnd ts = do
      (left, rest) <- parseNot ts
      parseAndTail left rest

    parseAndTail left (Token AndToken _ pos : rest) = do
      (right, afterRight) <- parseNot rest
      parseAndTail (BinaryExpr {binaryExprOp = AndOperator, binaryExprLeft = left, binaryExprRight = right, binaryExprPos = pos}) afterRight
    parseAndTail left rest = Right (left, rest)

    parseNot (Token NotToken _ pos : rest) = do
      (expr, remaining) <- parseNot rest
      Right (NotExpr {notExprValue = expr, notExprPos = pos}, remaining)
    parseNot ts = parseComparison ts

    parseComparison ts = do
      (left, rest) <- parseAdd ts
      parseComparisonTail left rest

    parseComparisonTail left (Token EqToken _ pos : rest) = do
      (right, afterRight) <- parseAdd rest
      parseComparisonTail (BinaryExpr {binaryExprOp = EqOperator, binaryExprLeft = left, binaryExprRight = right, binaryExprPos = pos}) afterRight
    parseComparisonTail left (Token NotEqToken _ pos : rest) = do
      (right, afterRight) <- parseAdd rest
      parseComparisonTail (BinaryExpr {binaryExprOp = NotEqOperator, binaryExprLeft = left, binaryExprRight = right, binaryExprPos = pos}) afterRight
    parseComparisonTail left (Token LtToken _ pos : rest) = do
      (right, afterRight) <- parseAdd rest
      parseComparisonTail (BinaryExpr {binaryExprOp = LtOperator, binaryExprLeft = left, binaryExprRight = right, binaryExprPos = pos}) afterRight
    parseComparisonTail left (Token GtToken _ pos : rest) = do
      (right, afterRight) <- parseAdd rest
      parseComparisonTail (BinaryExpr {binaryExprOp = GtOperator, binaryExprLeft = left, binaryExprRight = right, binaryExprPos = pos}) afterRight
    parseComparisonTail left (Token LteToken _ pos : rest) = do
      (right, afterRight) <- parseAdd rest
      parseComparisonTail (BinaryExpr {binaryExprOp = LteOperator, binaryExprLeft = left, binaryExprRight = right, binaryExprPos = pos}) afterRight
    parseComparisonTail left (Token GteToken _ pos : rest) = do
      (right, afterRight) <- parseAdd rest
      parseComparisonTail (BinaryExpr {binaryExprOp = GteOperator, binaryExprLeft = left, binaryExprRight = right, binaryExprPos = pos}) afterRight
    parseComparisonTail left remaining = Right (left, remaining)

    parseAdd tokenStream = do
      (leftExpr, remaining) <- parseMul tokenStream
      parseAddTail leftExpr remaining

    parseAddTail left (Token PlusToken _ pos : rest) = do
      (rightExpr, remaining) <- parseMul rest
      parseAddTail (BinaryExpr {binaryExprOp = AddOperator, binaryExprLeft = left, binaryExprRight = rightExpr, binaryExprPos = pos}) remaining
    parseAddTail left (Token MinusToken _ pos : rest) = do
      (rightExpr, remaining) <- parseMul rest
      parseAddTail (BinaryExpr {binaryExprOp = SubtractOperator, binaryExprLeft = left, binaryExprRight = rightExpr, binaryExprPos = pos}) remaining
    parseAddTail left remaining = Right (left, remaining)

    parseMul tokenStream = do
      (leftExpr, remaining) <- parsePrimary tokenStream
      parseMulTail leftExpr remaining

    parseMulTail left (Token StarToken _ pos : rest) = do
      (rightExpr, remaining) <- parsePrimary rest
      parseMulTail (BinaryExpr {binaryExprOp = MultiplyOperator, binaryExprLeft = left, binaryExprRight = rightExpr, binaryExprPos = pos}) remaining
    parseMulTail left (Token SlashToken _ pos : rest) = do
      (rightExpr, remaining) <- parsePrimary rest
      parseMulTail (BinaryExpr {binaryExprOp = DivideOperator, binaryExprLeft = left, binaryExprRight = rightExpr, binaryExprPos = pos}) remaining
    parseMulTail left (Token DoubleSlashToken _ pos : rest) = do
      (rightExpr, remaining) <- parsePrimary rest
      parseMulTail (BinaryExpr {binaryExprOp = FloorDivideOperator, binaryExprLeft = left, binaryExprRight = rightExpr, binaryExprPos = pos}) remaining
    parseMulTail left (Token PercentToken _ pos : rest) = do
      (rightExpr, remaining) <- parsePrimary rest
      parseMulTail (BinaryExpr {binaryExprOp = ModuloOperator, binaryExprLeft = left, binaryExprRight = rightExpr, binaryExprPos = pos}) remaining
    parseMulTail left remaining = Right (left, remaining)

    parsePrimary tokenStream = do
      (baseExpr, remaining) <- parseAtom tokenStream
      parsePostfix baseExpr remaining

    parseAtom (Token IntegerToken value pos : rest) = Right (IntegerExpr {integerExprValue = read value, integerExprPos = pos}, rest)
    parseAtom (Token FloatToken value pos : rest) = Right (FloatExpr {floatExprValue = read (normalizeFloatLiteral value), floatExprPos = pos}, rest)
    parseAtom (Token TrueToken _ pos : rest) = Right (IntegerExpr {integerExprValue = 1, integerExprPos = pos}, rest)
    parseAtom (Token FalseToken _ pos : rest) = Right (IntegerExpr {integerExprValue = 0, integerExprPos = pos}, rest)
    parseAtom (Token NoneToken _ pos : rest) = Right (NoneExpr {noneExprPos = pos}, rest)
    parseAtom (Token MinusToken _ pos : Token IntegerToken value _ : rest) = Right (IntegerExpr {integerExprValue = negate (read value), integerExprPos = pos}, rest)
    parseAtom (Token MinusToken _ pos : Token FloatToken value _ : rest) = Right (FloatExpr {floatExprValue = negate (read (normalizeFloatLiteral value)), floatExprPos = pos}, rest)
    parseAtom (Token MinusToken _ pos : rest) = do
      (expr, remaining) <- parsePrimary rest
      Right (UnaryMinusExpr {unaryMinusExprValue = expr, unaryMinusExprPos = pos}, remaining)
    parseAtom (Token StringToken value pos : rest) = Right (StringExpr {stringExprValue = value, stringExprPos = pos}, rest)
    parseAtom (Token LBracketToken _ pos : rest) = parseListElements pos rest
    parseAtom (Token LBraceToken _ pos : rest) = parseDictEntries pos rest
    parseAtom (Token LParenToken _ parenPos : rest) =
      parseParenTuple (ParseParenTupleConfig {parseParenTupleExpr = parseExpr, parseParenTuplePos = parenPos, parseParenTupleTokenStream = rest})
    parseAtom (Token IdentifierToken value pos : rest) = Right (IdentifierExpr {identifierExprName = value, identifierExprPos = pos}, rest)
    parseAtom (tok : _) = Left (ExpectedExpression {parseErrorPosition = position tok})
    parseAtom _ = Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})

    parsePostfix (IdentifierExpr name pos) (Token LParenToken _ _ : rest) = do
      (args, afterArgs) <- parseArguments rest
      parsePostfix (CallExpr {callExprName = name, callExprArgs = args, callExprPos = pos}) afterArgs
    parsePostfix calleeExpr (Token LParenToken _ _ : rest) = do
      (args, afterArgs) <- parseArguments rest
      parsePostfix (CallValueExpr {callValueExprCallee = calleeExpr, callValueExprArgs = args, callValueExprPos = exprPos calleeExpr}) afterArgs
    parsePostfix receiverExpr (Token DotToken _ _ : Token IdentifierToken methodName methodPos : Token LParenToken _ _ : rest) = do
      (args, afterArgs) <- parseArguments rest
      parsePostfix (CallExpr {callExprName = methodName, callExprArgs = receiverExpr : args, callExprPos = methodPos}) afterArgs
    parsePostfix receiverExpr (Token LBracketToken _ pos : rest) = do
      (subscriptExpr, afterSubscript) <- parseSubscriptExpr (ParseSubscriptExprConfig {parseSubscriptExprFn = parseExpr, parseSubscriptExprReceiver = receiverExpr, parseSubscriptExprPos = pos, parseSubscriptExprTokenStream = rest})
      parsePostfix subscriptExpr afterSubscript
    parsePostfix (IdentifierExpr receiverName receiverPos) (Token DotToken _ _ : Token IdentifierToken attrName _ : rest) =
      parsePostfix (IdentifierExpr {identifierExprName = receiverName ++ "." ++ attrName, identifierExprPos = receiverPos}) rest
    parsePostfix expr rest = Right (expr, rest)

    parseListElements listPos (Token RBracketToken _ _ : rest) =
      Right (ListExpr {listExprItems = [], listExprPos = listPos}, rest)
    parseListElements listPos ts = do
      (firstExpr, afterFirst) <- parseExpr ts
      case afterFirst of
        forTokens@(Token ForToken _ _ : _) ->
          parseComprehensionTail (ParseComprehensionTailConfig {parseComprehensionTailExpr = parseExpr, parseComprehensionTailValueExpr = firstExpr, parseComprehensionTailListPos = listPos, parseComprehensionTailClauses = [], parseComprehensionTailTokenStream = forTokens})
        _ -> parseListTail listPos [firstExpr] afterFirst

    parseListTail listPos exprs (Token CommaToken _ _ : rest) = do
      case rest of
        Token RBracketToken _ _ : afterBracket -> Right (ListExpr {listExprItems = exprs, listExprPos = listPos}, afterBracket)
        _ -> do
          (nextExpr, afterNext) <- parseExpr rest
          parseListTail listPos (exprs ++ [nextExpr]) afterNext
    parseListTail listPos exprs (Token RBracketToken _ _ : rest) =
      Right (ListExpr {listExprItems = exprs, listExprPos = listPos}, rest)
    parseListTail _ _ (tok : _) = Left (ExpectedExpression {parseErrorPosition = position tok})
    parseListTail _ _ _ = Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
    parseDictEntries dictPos (Token RBraceToken _ _ : rest) =
      Right (DictExpr {dictExprEntries = [], dictExprPos = dictPos}, rest)
    parseDictEntries dictPos ts = do
      (keyExpr, afterKey) <- parseExpr ts
      case afterKey of
        Token ColonToken _ _ : afterColon -> do
          (valueExpr, afterValue) <- parseExpr afterColon
          parseDictTail dictPos [(keyExpr, valueExpr)] afterValue
        tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
        _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
    parseDictTail dictPos pairs (Token CommaToken _ _ : rest) = do
      case rest of
        Token RBraceToken _ _ : afterBrace -> Right (DictExpr {dictExprEntries = pairs, dictExprPos = dictPos}, afterBrace)
        _ -> do
          (nextKey, afterKey) <- parseExpr rest
          case afterKey of
            Token ColonToken _ _ : afterColon -> do
              (nextValue, afterValue) <- parseExpr afterColon
              parseDictTail dictPos (pairs ++ [(nextKey, nextValue)]) afterValue
            tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
    parseDictTail dictPos pairs (Token RBraceToken _ _ : rest) =
      Right (DictExpr {dictExprEntries = pairs, dictExprPos = dictPos}, rest)
    parseDictTail _ _ (tok : _) = Left (ExpectedExpression {parseErrorPosition = position tok})
    parseDictTail _ _ _ = Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
    parseArguments (Token RParenToken _ _ : rest) = Right ([], rest)
    parseArguments ts = parseArgumentsTail False [] ts
    parseArgumentsTail seenKeywordArg accArgs tokenStream = do
      (argExpr, isKeywordArg, mismatchPos, afterArg) <- parseCallArgument (ParseCallArgumentConfig {parseCallArgumentExpr = parseExpr, parseCallArgumentTokenStream = tokenStream})
      if seenKeywordArg && not isKeywordArg
        then Left (ExpectedExpression {parseErrorPosition = mismatchPos})
        else case afterArg of
          Token RParenToken _ _ : rest -> Right (accArgs ++ [argExpr], rest)
          Token CommaToken _ _ : rest ->
            case rest of
              Token RParenToken _ _ : afterParen -> Right (accArgs ++ [argExpr], afterParen)
              _ -> parseArgumentsTail (seenKeywordArg || isKeywordArg) (accArgs ++ [argExpr]) rest
          Token _ _ pos : _ -> Left (ExpectedExpression {parseErrorPosition = pos})
          _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
