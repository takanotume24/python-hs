module PythonHS.Parser.ParsePattern (parsePattern) where

import PythonHS.AST.Expr (Expr (DictExpr, IntegerExpr, ListExpr, NoneExpr, StringExpr, FloatExpr))
import PythonHS.AST.Pattern (Pattern (AsPattern, CapturePattern, MappingPattern, OrPattern, SequencePattern, ValuePattern, WildcardPattern))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (AsToken, ColonToken, CommaToken, FalseToken, FloatToken, IdentifierToken, IntegerToken, LBraceToken, LBracketToken, NoneToken, PipeToken, RBraceToken, RBracketToken, StarToken, StringToken, TrueToken))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))
import PythonHS.Parser.ParsePatternConfig (ParsePatternConfig (..))

parsePattern :: ParsePatternConfig -> [Token] -> Either ParseError (Pattern, [Token])
parsePattern config tokens = do
  let parseExpr = parsePatternExpr config
  (firstPattern, afterFirst) <- parseSinglePattern parseExpr tokens
  (orPattern, afterOr) <- parseOrTail parseExpr firstPattern afterFirst
  parseAsTail orPattern afterOr
  where
    parseOrTail parseExpr left (Token PipeToken _ pos' : rest) = do
      (right, afterRight) <- parseSinglePattern parseExpr rest
      let merged =
            case left of
              OrPattern patterns _ -> patterns ++ [right]
              _ -> [left, right]
      parseOrTail parseExpr (OrPattern merged pos') afterRight
    parseOrTail _ left rest = Right (left, rest)

    parseAsTail patternValue (Token AsToken _ asPos : Token IdentifierToken aliasName _ : rest) =
      Right (AsPattern patternValue aliasName asPos, rest)
    parseAsTail patternValue rest = Right (patternValue, rest)

    parseSinglePattern _ (Token IdentifierToken "_" pos' : rest) = Right (WildcardPattern pos', rest)
    parseSinglePattern _ (Token IdentifierToken name pos' : rest) = Right (CapturePattern name pos', rest)
    parseSinglePattern _ (Token LBracketToken _ pos' : rest) = parseSequencePattern pos' rest
    parseSinglePattern _ (Token LBraceToken _ pos' : rest) = parseMappingPattern pos' rest
    parseSinglePattern parseExpr ts@(Token IntegerToken _ _ : _) = parseValuePattern parseExpr ts
    parseSinglePattern parseExpr ts@(Token FloatToken _ _ : _) = parseValuePattern parseExpr ts
    parseSinglePattern parseExpr ts@(Token StringToken _ _ : _) = parseValuePattern parseExpr ts
    parseSinglePattern parseExpr ts@(Token NoneToken _ _ : _) = parseValuePattern parseExpr ts
    parseSinglePattern parseExpr ts@(Token TrueToken _ _ : _) = parseValuePattern parseExpr ts
    parseSinglePattern parseExpr ts@(Token FalseToken _ _ : _) = parseValuePattern parseExpr ts
    parseSinglePattern _ (tok : _) = Left (ExpectedExpression (position tok))
    parseSinglePattern _ _ = Left (ExpectedExpression (Position 0 0))

    parseValuePattern parseExpr ts = do
      (expr, rest) <- parseExpr ts
      case expr of
        IntegerExpr _ pos' -> Right (ValuePattern expr pos', rest)
        FloatExpr _ pos' -> Right (ValuePattern expr pos', rest)
        StringExpr _ pos' -> Right (ValuePattern expr pos', rest)
        NoneExpr pos' -> Right (ValuePattern expr pos', rest)
        ListExpr _ pos' -> Right (ValuePattern expr pos', rest)
        DictExpr _ pos' -> Right (ValuePattern expr pos', rest)
        _ -> Left (ExpectedExpression (exprPos expr))

    parseSequencePattern seqPos (Token RBracketToken _ _ : rest) =
      Right (SequencePattern [] Nothing seqPos, rest)
    parseSequencePattern seqPos ts = parseSequenceItems seqPos [] Nothing ts

    parseSequenceItems seqPos acc restCapture (Token RBracketToken _ _ : rest) =
      Right (SequencePattern acc restCapture seqPos, rest)
    parseSequenceItems seqPos acc Nothing (Token StarToken _ _ : Token IdentifierToken "_" _ : afterRest) =
      parseAfterStar seqPos acc Nothing afterRest
    parseSequenceItems seqPos acc Nothing (Token StarToken _ _ : Token IdentifierToken name _ : afterRest) =
      parseAfterStar seqPos acc (Just name) afterRest
    parseSequenceItems seqPos acc restCapture stream = do
      (itemPattern, afterItem) <- parseSinglePattern (parsePatternExpr config) stream
      case afterItem of
        Token CommaToken _ _ : rest -> parseSequenceItems seqPos (acc ++ [itemPattern]) restCapture rest
        Token RBracketToken _ _ : rest -> Right (SequencePattern (acc ++ [itemPattern]) restCapture seqPos, rest)
        tok : _ -> Left (ExpectedExpression (position tok))
        _ -> Left (ExpectedExpression (Position 0 0))

    parseAfterStar seqPos acc restCapture (Token CommaToken _ _ : Token RBracketToken _ _ : rest) =
      Right (SequencePattern acc restCapture seqPos, rest)
    parseAfterStar seqPos acc restCapture (Token RBracketToken _ _ : rest) =
      Right (SequencePattern acc restCapture seqPos, rest)
    parseAfterStar _ _ _ (tok : _) = Left (ExpectedExpression (position tok))
    parseAfterStar _ _ _ _ = Left (ExpectedExpression (Position 0 0))

    parseMappingPattern mapPos (Token RBraceToken _ _ : rest) =
      Right (MappingPattern [] Nothing mapPos, rest)
    parseMappingPattern mapPos ts = parseEntries mapPos [] Nothing ts

    parseEntries mapPos acc maybeRestCapture stream =
      case stream of
        Token StarToken _ _ : Token StarToken _ _ : Token IdentifierToken restName _ : afterRest ->
          parseAfterDoubleStar mapPos (acc, Just restName) afterRest
        _ -> do
          (keyExpr, afterKey) <- (parsePatternExpr config) stream
          case afterKey of
            Token ColonToken _ _ : afterColon -> do
              (valuePattern, afterValue) <- parseSinglePattern (parsePatternExpr config) afterColon
              case afterValue of
                Token CommaToken _ _ : rest -> parseEntries mapPos (acc ++ [(keyExpr, valuePattern)]) maybeRestCapture rest
                Token RBraceToken _ _ : rest -> Right (MappingPattern (acc ++ [(keyExpr, valuePattern)]) maybeRestCapture mapPos, rest)
                tok : _ -> Left (ExpectedExpression (position tok))
                _ -> Left (ExpectedExpression (Position 0 0))
            tok : _ -> Left (ExpectedExpression (position tok))
            _ -> Left (ExpectedExpression (Position 0 0))

    parseAfterDoubleStar mapPos (acc, maybeRestCapture) afterRest =
      case afterRest of
        Token CommaToken _ _ : Token RBraceToken _ _ : rest -> Right (MappingPattern acc maybeRestCapture mapPos, rest)
        Token RBraceToken _ _ : rest -> Right (MappingPattern acc maybeRestCapture mapPos, rest)
        tok : _ -> Left (ExpectedExpression (position tok))
        _ -> Left (ExpectedExpression (Position 0 0))

    exprPos (IntegerExpr _ pos') = pos'
    exprPos (FloatExpr _ pos') = pos'
    exprPos (StringExpr _ pos') = pos'
    exprPos (NoneExpr pos') = pos'
    exprPos (ListExpr _ pos') = pos'
    exprPos (DictExpr _ pos') = pos'
    exprPos _ = Position 0 0
