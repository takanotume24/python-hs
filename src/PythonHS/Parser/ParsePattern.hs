module PythonHS.Parser.ParsePattern (parsePattern) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.MappingPatternEntry (MappingPatternEntry (..))
import PythonHS.AST.Pattern (Pattern (..))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType (TokenType (AsToken, ColonToken, CommaToken, FalseToken, FloatToken, IdentifierToken, IntegerToken, LBraceToken, LBracketToken, NoneToken, PipeToken, RBraceToken, RBracketToken, StarToken, StringToken, TrueToken))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParsePatternConfig (ParsePatternConfig (..))
import PythonHS.Parser.ParseResult (ParseResult (..))

parsePattern :: ParsePatternConfig -> Either ParseError (ParseResult Pattern)
parsePattern config = do
  let parseExpr = parsePatternExpr config
      tokens = parsePatternTokenStream config
  firstResult <- parseSinglePattern parseExpr tokens
  let firstPattern = parseResultValue firstResult
      afterFirst = parseResultRemaining firstResult
  orResult <- parseOrTail parseExpr firstPattern afterFirst
  let orPattern = parseResultValue orResult
      afterOr = parseResultRemaining orResult
  parseAsTail orPattern afterOr
  where
    parseOrTail parseExpr left (Token {tokenType = PipeToken, position = pos'} : rest) = do
      rightResult <- parseSinglePattern parseExpr rest
      let right = parseResultValue rightResult
          afterRight = parseResultRemaining rightResult
      let merged =
            case left of
              OrPattern {orPatternItems = patterns} -> patterns ++ [right]
              _ -> [left, right]
      parseOrTail parseExpr (OrPattern {orPatternItems = merged, orPatternPos = pos'}) afterRight
    parseOrTail _ left rest = Right (ParseResult {parseResultValue = left, parseResultRemaining = rest})

    parseAsTail patternValue (Token {tokenType = AsToken, position = asPos} : Token {tokenType = IdentifierToken, lexeme = aliasName} : rest) =
      Right (ParseResult {parseResultValue = AsPattern {asPatternInner = patternValue, asPatternAlias = aliasName, asPatternPos = asPos}, parseResultRemaining = rest})
    parseAsTail patternValue rest = Right (ParseResult {parseResultValue = patternValue, parseResultRemaining = rest})

    parseSinglePattern _ (Token {tokenType = IdentifierToken, lexeme = "_", position = pos'} : rest) = Right (ParseResult {parseResultValue = WildcardPattern {wildcardPatternPos = pos'}, parseResultRemaining = rest})
    parseSinglePattern _ (Token {tokenType = IdentifierToken, lexeme = name, position = pos'} : rest) = Right (ParseResult {parseResultValue = CapturePattern {capturePatternName = name, capturePatternPos = pos'}, parseResultRemaining = rest})
    parseSinglePattern _ (Token {tokenType = LBracketToken, position = pos'} : rest) = parseSequencePattern pos' rest
    parseSinglePattern _ (Token {tokenType = LBraceToken, position = pos'} : rest) = parseMappingPattern pos' rest
    parseSinglePattern parseExpr ts@(Token {tokenType = IntegerToken} : _) = parseValuePattern parseExpr ts
    parseSinglePattern parseExpr ts@(Token {tokenType = FloatToken} : _) = parseValuePattern parseExpr ts
    parseSinglePattern parseExpr ts@(Token {tokenType = StringToken} : _) = parseValuePattern parseExpr ts
    parseSinglePattern parseExpr ts@(Token {tokenType = NoneToken} : _) = parseValuePattern parseExpr ts
    parseSinglePattern parseExpr ts@(Token {tokenType = TrueToken} : _) = parseValuePattern parseExpr ts
    parseSinglePattern parseExpr ts@(Token {tokenType = FalseToken} : _) = parseValuePattern parseExpr ts
    parseSinglePattern _ (tok : _) = Left (ExpectedExpression {parseErrorPosition = position tok})
    parseSinglePattern _ _ = Left (ExpectedExpression {parseErrorPosition = Position 0 0})

    parseValuePattern parseExpr ts = do
      exprResult <- parseExpr ts
      let expr = parseResultValue exprResult
          rest = parseResultRemaining exprResult
      case expr of
        IntegerExpr {integerExprPos = pos'} -> Right (ParseResult {parseResultValue = ValuePattern {valuePatternExpr = expr, valuePatternPos = pos'}, parseResultRemaining = rest})
        FloatExpr {floatExprPos = pos'} -> Right (ParseResult {parseResultValue = ValuePattern {valuePatternExpr = expr, valuePatternPos = pos'}, parseResultRemaining = rest})
        StringExpr {stringExprPos = pos'} -> Right (ParseResult {parseResultValue = ValuePattern {valuePatternExpr = expr, valuePatternPos = pos'}, parseResultRemaining = rest})
        NoneExpr {noneExprPos = pos'} -> Right (ParseResult {parseResultValue = ValuePattern {valuePatternExpr = expr, valuePatternPos = pos'}, parseResultRemaining = rest})
        ListExpr {listExprPos = pos'} -> Right (ParseResult {parseResultValue = ValuePattern {valuePatternExpr = expr, valuePatternPos = pos'}, parseResultRemaining = rest})
        DictExpr {dictExprPos = pos'} -> Right (ParseResult {parseResultValue = ValuePattern {valuePatternExpr = expr, valuePatternPos = pos'}, parseResultRemaining = rest})
        _ -> Left (ExpectedExpression {parseErrorPosition = exprPos expr})

    parseSequencePattern seqPos (Token {tokenType = RBracketToken} : rest) =
      Right (ParseResult {parseResultValue = SequencePattern {sequencePatternItems = [], sequencePatternRest = Nothing, sequencePatternPos = seqPos}, parseResultRemaining = rest})
    parseSequencePattern seqPos ts = parseSequenceItems seqPos [] Nothing ts

    parseSequenceItems seqPos acc restCapture (Token {tokenType = RBracketToken} : rest) =
      Right (ParseResult {parseResultValue = SequencePattern {sequencePatternItems = acc, sequencePatternRest = restCapture, sequencePatternPos = seqPos}, parseResultRemaining = rest})
    parseSequenceItems seqPos acc Nothing (Token {tokenType = StarToken} : Token {tokenType = IdentifierToken, lexeme = "_"} : afterRest) =
      parseAfterStar seqPos acc Nothing afterRest
    parseSequenceItems seqPos acc Nothing (Token {tokenType = StarToken} : Token {tokenType = IdentifierToken, lexeme = name} : afterRest) =
      parseAfterStar seqPos acc (Just name) afterRest
    parseSequenceItems seqPos acc restCapture stream = do
      itemResult <- parseSinglePattern (parsePatternExpr config) stream
      let itemPattern = parseResultValue itemResult
          afterItem = parseResultRemaining itemResult
      case afterItem of
        Token {tokenType = CommaToken} : rest -> parseSequenceItems seqPos (acc ++ [itemPattern]) restCapture rest
        Token {tokenType = RBracketToken} : rest -> Right (ParseResult {parseResultValue = SequencePattern {sequencePatternItems = acc ++ [itemPattern], sequencePatternRest = restCapture, sequencePatternPos = seqPos}, parseResultRemaining = rest})
        tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
        _ -> Left (ExpectedExpression {parseErrorPosition = Position 0 0})

    parseAfterStar seqPos acc restCapture (Token {tokenType = CommaToken} : Token {tokenType = RBracketToken} : rest) =
      Right (ParseResult {parseResultValue = SequencePattern {sequencePatternItems = acc, sequencePatternRest = restCapture, sequencePatternPos = seqPos}, parseResultRemaining = rest})
    parseAfterStar seqPos acc restCapture (Token {tokenType = RBracketToken} : rest) =
      Right (ParseResult {parseResultValue = SequencePattern {sequencePatternItems = acc, sequencePatternRest = restCapture, sequencePatternPos = seqPos}, parseResultRemaining = rest})
    parseAfterStar _ _ _ (tok : _) = Left (ExpectedExpression {parseErrorPosition = position tok})
    parseAfterStar _ _ _ _ = Left (ExpectedExpression {parseErrorPosition = Position 0 0})

    parseMappingPattern mapPos (Token {tokenType = RBraceToken} : rest) =
      Right (ParseResult {parseResultValue = MappingPattern {mappingPatternPairs = [], mappingPatternRest = Nothing, mappingPatternPos = mapPos}, parseResultRemaining = rest})
    parseMappingPattern mapPos ts = parseEntries mapPos [] Nothing ts

    parseEntries mapPos acc maybeRestCapture stream =
      case stream of
        Token {tokenType = StarToken} : Token {tokenType = StarToken} : Token {tokenType = IdentifierToken, lexeme = restName} : afterRest ->
          parseAfterDoubleStar mapPos acc (Just restName) afterRest
        _ -> do
          keyResult <- (parsePatternExpr config) stream
          let keyExpr = parseResultValue keyResult
              afterKey = parseResultRemaining keyResult
          case afterKey of
            Token {tokenType = ColonToken} : afterColon -> do
              valueResult <- parseSinglePattern (parsePatternExpr config) afterColon
              let valuePattern = parseResultValue valueResult
                  afterValue = parseResultRemaining valueResult
              case afterValue of
                Token {tokenType = CommaToken} : rest -> parseEntries mapPos (acc ++ [MappingPatternEntry {mappingPatternEntryKey = keyExpr, mappingPatternEntryValue = valuePattern}]) maybeRestCapture rest
                Token {tokenType = RBraceToken} : rest -> Right (ParseResult {parseResultValue = MappingPattern {mappingPatternPairs = acc ++ [MappingPatternEntry {mappingPatternEntryKey = keyExpr, mappingPatternEntryValue = valuePattern}], mappingPatternRest = maybeRestCapture, mappingPatternPos = mapPos}, parseResultRemaining = rest})
                tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
                _ -> Left (ExpectedExpression {parseErrorPosition = Position 0 0})
            tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position 0 0})

    parseAfterDoubleStar mapPos acc maybeRestCapture afterRest =
      case afterRest of
        Token {tokenType = CommaToken} : Token {tokenType = RBraceToken} : rest -> Right (ParseResult {parseResultValue = MappingPattern {mappingPatternPairs = acc, mappingPatternRest = maybeRestCapture, mappingPatternPos = mapPos}, parseResultRemaining = rest})
        Token {tokenType = RBraceToken} : rest -> Right (ParseResult {parseResultValue = MappingPattern {mappingPatternPairs = acc, mappingPatternRest = maybeRestCapture, mappingPatternPos = mapPos}, parseResultRemaining = rest})
        tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
        _ -> Left (ExpectedExpression {parseErrorPosition = Position 0 0})

    exprPos IntegerExpr {integerExprPos = pos'} = pos'
    exprPos FloatExpr {floatExprPos = pos'} = pos'
    exprPos StringExpr {stringExprPos = pos'} = pos'
    exprPos NoneExpr {noneExprPos = pos'} = pos'
    exprPos ListExpr {listExprPos = pos'} = pos'
    exprPos DictExpr {dictExprPos = pos'} = pos'
    exprPos _ = Position 0 0
