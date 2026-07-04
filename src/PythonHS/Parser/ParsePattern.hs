module PythonHS.Parser.ParsePattern (parsePattern) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Pattern (Pattern (..))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType (TokenType (AsToken, ColonToken, CommaToken, FalseToken, FloatToken, IdentifierToken, IntegerToken, LBraceToken, LBracketToken, NoneToken, PipeToken, RBraceToken, RBracketToken, StarToken, StringToken, TrueToken))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParsePatternConfig (ParsePatternConfig (..))

parsePattern :: ParsePatternConfig -> [Token] -> Either ParseError (Pattern, [Token])
parsePattern config tokens = do
  let parseExpr = parsePatternExpr config
  (firstPattern, afterFirst) <- parseSinglePattern parseExpr tokens
  (orPattern, afterOr) <- parseOrTail parseExpr firstPattern afterFirst
  parseAsTail orPattern afterOr
  where
    parseOrTail parseExpr left (Token {tokenType = PipeToken, position = pos'} : rest) = do
      (right, afterRight) <- parseSinglePattern parseExpr rest
      let merged =
            case left of
              OrPattern {orPatternItems = patterns} -> patterns ++ [right]
              _ -> [left, right]
      parseOrTail parseExpr (OrPattern {orPatternItems = merged, orPatternPos = pos'}) afterRight
    parseOrTail _ left rest = Right (left, rest)

    parseAsTail patternValue (Token {tokenType = AsToken, position = asPos} : Token {tokenType = IdentifierToken, lexeme = aliasName} : rest) =
      Right (AsPattern {asPatternInner = patternValue, asPatternAlias = aliasName, asPatternPos = asPos}, rest)
    parseAsTail patternValue rest = Right (patternValue, rest)

    parseSinglePattern _ (Token {tokenType = IdentifierToken, lexeme = "_", position = pos'} : rest) = Right (WildcardPattern {wildcardPatternPos = pos'}, rest)
    parseSinglePattern _ (Token {tokenType = IdentifierToken, lexeme = name, position = pos'} : rest) = Right (CapturePattern {capturePatternName = name, capturePatternPos = pos'}, rest)
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
      (expr, rest) <- parseExpr ts
      case expr of
        IntegerExpr {integerExprPos = pos'} -> Right (ValuePattern {valuePatternExpr = expr, valuePatternPos = pos'}, rest)
        FloatExpr {floatExprPos = pos'} -> Right (ValuePattern {valuePatternExpr = expr, valuePatternPos = pos'}, rest)
        StringExpr {stringExprPos = pos'} -> Right (ValuePattern {valuePatternExpr = expr, valuePatternPos = pos'}, rest)
        NoneExpr {noneExprPos = pos'} -> Right (ValuePattern {valuePatternExpr = expr, valuePatternPos = pos'}, rest)
        ListExpr {listExprPos = pos'} -> Right (ValuePattern {valuePatternExpr = expr, valuePatternPos = pos'}, rest)
        DictExpr {dictExprPos = pos'} -> Right (ValuePattern {valuePatternExpr = expr, valuePatternPos = pos'}, rest)
        _ -> Left (ExpectedExpression {parseErrorPosition = exprPos expr})

    parseSequencePattern seqPos (Token {tokenType = RBracketToken} : rest) =
      Right (SequencePattern {sequencePatternItems = [], sequencePatternRest = Nothing, sequencePatternPos = seqPos}, rest)
    parseSequencePattern seqPos ts = parseSequenceItems seqPos [] Nothing ts

    parseSequenceItems seqPos acc restCapture (Token {tokenType = RBracketToken} : rest) =
      Right (SequencePattern {sequencePatternItems = acc, sequencePatternRest = restCapture, sequencePatternPos = seqPos}, rest)
    parseSequenceItems seqPos acc Nothing (Token {tokenType = StarToken} : Token {tokenType = IdentifierToken, lexeme = "_"} : afterRest) =
      parseAfterStar seqPos acc Nothing afterRest
    parseSequenceItems seqPos acc Nothing (Token {tokenType = StarToken} : Token {tokenType = IdentifierToken, lexeme = name} : afterRest) =
      parseAfterStar seqPos acc (Just name) afterRest
    parseSequenceItems seqPos acc restCapture stream = do
      (itemPattern, afterItem) <- parseSinglePattern (parsePatternExpr config) stream
      case afterItem of
        Token {tokenType = CommaToken} : rest -> parseSequenceItems seqPos (acc ++ [itemPattern]) restCapture rest
        Token {tokenType = RBracketToken} : rest -> Right (SequencePattern {sequencePatternItems = acc ++ [itemPattern], sequencePatternRest = restCapture, sequencePatternPos = seqPos}, rest)
        tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
        _ -> Left (ExpectedExpression {parseErrorPosition = Position 0 0})

    parseAfterStar seqPos acc restCapture (Token {tokenType = CommaToken} : Token {tokenType = RBracketToken} : rest) =
      Right (SequencePattern {sequencePatternItems = acc, sequencePatternRest = restCapture, sequencePatternPos = seqPos}, rest)
    parseAfterStar seqPos acc restCapture (Token {tokenType = RBracketToken} : rest) =
      Right (SequencePattern {sequencePatternItems = acc, sequencePatternRest = restCapture, sequencePatternPos = seqPos}, rest)
    parseAfterStar _ _ _ (tok : _) = Left (ExpectedExpression {parseErrorPosition = position tok})
    parseAfterStar _ _ _ _ = Left (ExpectedExpression {parseErrorPosition = Position 0 0})

    parseMappingPattern mapPos (Token {tokenType = RBraceToken} : rest) =
      Right (MappingPattern {mappingPatternPairs = [], mappingPatternRest = Nothing, mappingPatternPos = mapPos}, rest)
    parseMappingPattern mapPos ts = parseEntries mapPos [] Nothing ts

    parseEntries mapPos acc maybeRestCapture stream =
      case stream of
        Token {tokenType = StarToken} : Token {tokenType = StarToken} : Token {tokenType = IdentifierToken, lexeme = restName} : afterRest ->
          parseAfterDoubleStar mapPos (acc, Just restName) afterRest
        _ -> do
          (keyExpr, afterKey) <- (parsePatternExpr config) stream
          case afterKey of
            Token {tokenType = ColonToken} : afterColon -> do
              (valuePattern, afterValue) <- parseSinglePattern (parsePatternExpr config) afterColon
              case afterValue of
                Token {tokenType = CommaToken} : rest -> parseEntries mapPos (acc ++ [(keyExpr, valuePattern)]) maybeRestCapture rest
                Token {tokenType = RBraceToken} : rest -> Right (MappingPattern {mappingPatternPairs = acc ++ [(keyExpr, valuePattern)], mappingPatternRest = maybeRestCapture, mappingPatternPos = mapPos}, rest)
                tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
                _ -> Left (ExpectedExpression {parseErrorPosition = Position 0 0})
            tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position 0 0})

    parseAfterDoubleStar mapPos (acc, maybeRestCapture) afterRest =
      case afterRest of
        Token {tokenType = CommaToken} : Token {tokenType = RBraceToken} : rest -> Right (MappingPattern {mappingPatternPairs = acc, mappingPatternRest = maybeRestCapture, mappingPatternPos = mapPos}, rest)
        Token {tokenType = RBraceToken} : rest -> Right (MappingPattern {mappingPatternPairs = acc, mappingPatternRest = maybeRestCapture, mappingPatternPos = mapPos}, rest)
        tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
        _ -> Left (ExpectedExpression {parseErrorPosition = Position 0 0})

    exprPos IntegerExpr {integerExprPos = pos'} = pos'
    exprPos FloatExpr {floatExprPos = pos'} = pos'
    exprPos StringExpr {stringExprPos = pos'} = pos'
    exprPos NoneExpr {noneExprPos = pos'} = pos'
    exprPos ListExpr {listExprPos = pos'} = pos'
    exprPos DictExpr {dictExprPos = pos'} = pos'
    exprPos _ = Position 0 0
