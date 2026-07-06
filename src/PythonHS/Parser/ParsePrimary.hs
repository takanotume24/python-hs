module PythonHS.Parser.ParsePrimary (parsePrimary) where

import PythonHS.AST.DictEntry (DictEntry (..))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType
  ( TokenType
      ( ColonToken,
        CommaToken,
        DotToken,
        FalseToken,
        FloatToken,
        ForToken,
        IdentifierToken,
        IntegerToken,
        LBraceToken,
        LBracketToken,
        LParenToken,
        MinusToken,
        NoneToken,
        RBraceToken,
        RBracketToken,
        RParenToken,
        StringToken,
        TrueToken
      ),
  )
import PythonHS.Parser.ExprPos (exprPos)
import PythonHS.Parser.NormalizeFloatLiteral (normalizeFloatLiteral)
import PythonHS.Parser.ParseCallArgument (parseCallArgument)
import PythonHS.Parser.ParseCallArgumentConfig (ParseCallArgumentConfig (..))
import PythonHS.Parser.ParseCallArgumentResult (ParseCallArgumentResult (..))
import PythonHS.Parser.ParseComprehensionTail (parseComprehensionTail)
import PythonHS.Parser.ParseComprehensionTailConfig (ParseComprehensionTailConfig (..))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseParenTuple (parseParenTuple)
import PythonHS.Parser.ParseParenTupleConfig (ParseParenTupleConfig (..))
import PythonHS.Parser.ParsePrimaryConfig (ParsePrimaryConfig (..))
import PythonHS.Parser.ParseResult (ParseResult (..))
import PythonHS.Parser.ParseSubscriptExpr (parseSubscriptExpr)
import PythonHS.Parser.ParseSubscriptExprConfig (ParseSubscriptExprConfig (..))

parsePrimary :: ParsePrimaryConfig -> Either ParseError (ParseResult Expr)
parsePrimary config = do
  atomResult <- parseAtom (parsePrimaryTokenStream config)
  let baseExpr = parseResultValue atomResult
      remaining = parseResultRemaining atomResult
  parsePostfix baseExpr remaining
  where
    parseAtom (Token IntegerToken value pos : rest) = Right (ParseResult {parseResultValue = IntegerExpr {integerExprValue = read value, integerExprPos = pos}, parseResultRemaining = rest})
    parseAtom (Token FloatToken value pos : rest) = Right (ParseResult {parseResultValue = FloatExpr {floatExprValue = read (normalizeFloatLiteral value), floatExprPos = pos}, parseResultRemaining = rest})
    parseAtom (Token TrueToken _ pos : rest) = Right (ParseResult {parseResultValue = IntegerExpr {integerExprValue = 1, integerExprPos = pos}, parseResultRemaining = rest})
    parseAtom (Token FalseToken _ pos : rest) = Right (ParseResult {parseResultValue = IntegerExpr {integerExprValue = 0, integerExprPos = pos}, parseResultRemaining = rest})
    parseAtom (Token NoneToken _ pos : rest) = Right (ParseResult {parseResultValue = NoneExpr {noneExprPos = pos}, parseResultRemaining = rest})
    parseAtom (Token MinusToken _ pos : Token IntegerToken value _ : rest) = Right (ParseResult {parseResultValue = IntegerExpr {integerExprValue = negate (read value), integerExprPos = pos}, parseResultRemaining = rest})
    parseAtom (Token MinusToken _ pos : Token FloatToken value _ : rest) = Right (ParseResult {parseResultValue = FloatExpr {floatExprValue = negate (read (normalizeFloatLiteral value)), floatExprPos = pos}, parseResultRemaining = rest})
    parseAtom (Token MinusToken _ pos : rest) = do
      primaryResult <- parsePrimary (config {parsePrimaryTokenStream = rest})
      let expr = parseResultValue primaryResult
          remaining = parseResultRemaining primaryResult
      Right (ParseResult {parseResultValue = UnaryMinusExpr {unaryMinusExprValue = expr, unaryMinusExprPos = pos}, parseResultRemaining = remaining})
    parseAtom (Token StringToken value pos : rest) = Right (ParseResult {parseResultValue = StringExpr {stringExprValue = value, stringExprPos = pos}, parseResultRemaining = rest})
    parseAtom (Token LBracketToken _ pos : rest) = parseListElements pos rest
    parseAtom (Token LBraceToken _ pos : rest) = parseDictEntries pos rest
    parseAtom (Token LParenToken _ parenPos : rest) =
      parseParenTuple (ParseParenTupleConfig {parseParenTupleExpr = parsePrimaryExpr config, parseParenTuplePos = parenPos, parseParenTupleTokenStream = rest})
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
      subResult <- parseSubscriptExpr (ParseSubscriptExprConfig {parseSubscriptExprFn = parsePrimaryExpr config, parseSubscriptExprReceiver = receiverExpr, parseSubscriptExprPos = pos, parseSubscriptExprTokenStream = rest})
      let subscriptExpr = parseResultValue subResult
          afterSubscript = parseResultRemaining subResult
      parsePostfix subscriptExpr afterSubscript
    parsePostfix (IdentifierExpr receiverName receiverPos) (Token DotToken _ _ : Token IdentifierToken attrName _ : rest) =
      parsePostfix (IdentifierExpr {identifierExprName = receiverName ++ "." ++ attrName, identifierExprPos = receiverPos}) rest
    parsePostfix expr rest = Right (ParseResult {parseResultValue = expr, parseResultRemaining = rest})

    parseListElements listPos (Token RBracketToken _ _ : rest) =
      Right (ParseResult {parseResultValue = ListExpr {listExprItems = [], listExprPos = listPos}, parseResultRemaining = rest})
    parseListElements listPos ts = do
      firstResult <- parsePrimaryExpr config ts
      let firstExpr = parseResultValue firstResult
          afterFirst = parseResultRemaining firstResult
      case afterFirst of
        forTokens@(Token ForToken _ _ : _) ->
          parseComprehensionTail (ParseComprehensionTailConfig {parseComprehensionTailExpr = parsePrimaryExpr config, parseComprehensionTailValueExpr = firstExpr, parseComprehensionTailListPos = listPos, parseComprehensionTailClauses = [], parseComprehensionTailTokenStream = forTokens})
        _ -> parseListTail listPos [firstExpr] afterFirst

    parseListTail listPos exprs (Token CommaToken _ _ : rest) = do
      case rest of
        Token RBracketToken _ _ : afterBracket -> Right (ParseResult {parseResultValue = ListExpr {listExprItems = exprs, listExprPos = listPos}, parseResultRemaining = afterBracket})
        _ -> do
          nextResult <- parsePrimaryExpr config rest
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
      keyResult <- parsePrimaryExpr config ts
      let keyExpr = parseResultValue keyResult
          afterKey = parseResultRemaining keyResult
      case afterKey of
        Token ColonToken _ _ : afterColon -> do
          valueResult <- parsePrimaryExpr config afterColon
          let valueExpr = parseResultValue valueResult
              afterValue = parseResultRemaining valueResult
          parseDictTail dictPos [DictEntry {dictEntryKey = keyExpr, dictEntryValue = valueExpr}] afterValue
        tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
        _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})

    parseDictTail dictPos pairs (Token CommaToken _ _ : rest) = do
      case rest of
        Token RBraceToken _ _ : afterBrace -> Right (ParseResult {parseResultValue = DictExpr {dictExprEntries = pairs, dictExprPos = dictPos}, parseResultRemaining = afterBrace})
        _ -> do
          nextKeyResult <- parsePrimaryExpr config rest
          let nextKey = parseResultValue nextKeyResult
              afterKey = parseResultRemaining nextKeyResult
          case afterKey of
            Token ColonToken _ _ : afterColon -> do
              nextValueResult <- parsePrimaryExpr config afterColon
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
      argResult <- parseCallArgument (ParseCallArgumentConfig {parseCallArgumentExpr = parsePrimaryExpr config, parseCallArgumentTokenStream = tokenStream})
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
