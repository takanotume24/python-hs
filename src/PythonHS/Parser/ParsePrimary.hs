module PythonHS.Parser.ParsePrimary (parsePrimary) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType
  ( TokenType
      ( BytesToken,
        CommaToken,
        DotToken,
        FStringToken,
        FalseToken,
        FloatToken,
        IdentifierToken,
        IntegerToken,
        LBraceToken,
        LBracketToken,
        LParenToken,
        MinusToken,
        NoneToken,
        RBraceToken,
        RParenToken,
        StringToken,
        TildeToken,
        TrueToken
      ),
  )
import PythonHS.Parser.DesugarStarredTuple (desugarStarredTuple)
import PythonHS.Parser.ExprPos (exprPos)
import PythonHS.Parser.NormalizeFloatLiteral (normalizeFloatLiteral)
import PythonHS.Parser.ParseCallArgument (parseCallArgument)
import PythonHS.Parser.ParseCallArgumentConfig (ParseCallArgumentConfig (..))
import PythonHS.Parser.ParseCallArgumentResult (ParseCallArgumentResult (..))
import PythonHS.Parser.ParseComprehensionTail (parseComprehensionTail)
import PythonHS.Parser.ParseComprehensionTailConfig (ParseComprehensionTailConfig (..))
import PythonHS.Parser.ParseDictDisplay (parseDictDisplay)
import PythonHS.Parser.ParseDictDisplayConfig (ParseDictDisplayConfig (..))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseFStringExpr (parseFStringExpr)
import PythonHS.Parser.ParseFStringExprConfig (ParseFStringExprConfig (..))
import PythonHS.Parser.ParseListDisplay (parseListDisplay)
import PythonHS.Parser.ParseListDisplayConfig (ParseListDisplayConfig (..))
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
    parseAtom (Token TildeToken _ pos : rest) = do
      primaryResult <- parsePrimary (config {parsePrimaryTokenStream = rest})
      let expr = parseResultValue primaryResult
          remaining = parseResultRemaining primaryResult
      Right (ParseResult {parseResultValue = BitwiseNotExpr {bitwiseNotExprValue = expr, bitwiseNotExprPos = pos}, parseResultRemaining = remaining})
    parseAtom (Token StringToken value pos : rest) =
      let (adjacentValues, remaining) = collectAdjacentStrings rest
       in Right (ParseResult {parseResultValue = StringExpr {stringExprValue = concat (value : adjacentValues), stringExprPos = pos}, parseResultRemaining = remaining})
    parseAtom (Token BytesToken value pos : rest) =
      let (adjacentValues, remaining) = collectAdjacentBytes rest
          sourceText = concat (value : adjacentValues)
          textExpr = StringExpr {stringExprValue = sourceText, stringExprPos = pos}
          encodingExpr = StringExpr {stringExprValue = "latin-1", stringExprPos = pos}
       in Right
            ( ParseResult
                { parseResultValue = CallExpr {callExprName = "bytes", callExprArgs = [textExpr, encodingExpr], callExprPos = pos},
                  parseResultRemaining = remaining
                }
            )
    parseAtom (Token FStringToken value pos : rest) = do
      expr <- parseFStringExpr ParseFStringExprConfig {parseFStringExprParseExpr = parsePrimaryExpr config, parseFStringExprContent = value, parseFStringExprPos = pos}
      Right (ParseResult {parseResultValue = expr, parseResultRemaining = rest})
    parseAtom (Token LBracketToken _ pos : rest) =
      parseListDisplay ParseListDisplayConfig {parseListDisplayExpr = parsePrimaryExpr config, parseListDisplayPos = pos, parseListDisplayTokenStream = rest}
    parseAtom (Token LBraceToken _ pos : rest) =
      parseDictDisplay
        ParseDictDisplayConfig
          { parseDictDisplayExpr = parsePrimaryExpr config,
            parseDictDisplayComprehension = \valueExpr comprehensionTokens ->
              parseComprehensionTail
                ParseComprehensionTailConfig
                  { parseComprehensionTailExpr = parsePrimaryExpr config,
                    parseComprehensionTailValueExpr = valueExpr,
                    parseComprehensionTailListPos = pos,
                    parseComprehensionTailClauses = [],
                    parseComprehensionTailClosingToken = RBraceToken,
                    parseComprehensionTailTokenStream = comprehensionTokens
                  },
            parseDictDisplayPos = pos,
            parseDictDisplayTokenStream = rest
          }
    parseAtom (Token LParenToken _ parenPos : rest) = do
      tupleResult <- parseParenTuple (ParseParenTupleConfig {parseParenTupleExpr = parsePrimaryExpr config, parseParenTuplePos = parenPos, parseParenTupleTokenStream = rest})
      let tupleExpr = parseResultValue tupleResult
      case tupleExpr of
        TupleExpr {tupleExprItems = items, tupleExprPos = pos} ->
          Right (ParseResult {parseResultValue = desugarStarredTuple pos items, parseResultRemaining = parseResultRemaining tupleResult})
        _ -> Right tupleResult
    parseAtom (Token IdentifierToken value pos : rest) = Right (ParseResult {parseResultValue = IdentifierExpr {identifierExprName = value, identifierExprPos = pos}, parseResultRemaining = rest})
    parseAtom (tok : _) = Left (ExpectedExpression {parseErrorPosition = position tok})
    parseAtom _ = Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})

    collectAdjacentStrings (Token StringToken value _ : rest) =
      let (values, remaining) = collectAdjacentStrings rest
       in (value : values, remaining)
    collectAdjacentStrings rest = ([], rest)

    collectAdjacentBytes (Token BytesToken value _ : rest) =
      let (values, remaining) = collectAdjacentBytes rest
       in (value : values, remaining)
    collectAdjacentBytes rest = ([], rest)

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
