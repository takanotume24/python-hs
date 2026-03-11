module PythonHS.Parser.ParsePattern (parsePattern) where

import PythonHS.AST.Expr (Expr (DictExpr, IntegerExpr, ListExpr, NoneExpr, StringExpr, FloatExpr))
import PythonHS.AST.Pattern (Pattern (AsPattern, CapturePattern, MappingPattern, OrPattern, SequencePattern, ValuePattern, WildcardPattern))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (AsToken, ColonToken, CommaToken, FalseToken, FloatToken, IdentifierToken, IntegerToken, LBraceToken, LBracketToken, NoneToken, PipeToken, RBraceToken, RBracketToken, StarToken, StringToken, TrueToken))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))

parsePattern :: ([Token] -> Either ParseError (Expr, [Token])) -> [Token] -> Either ParseError (Pattern, [Token])
parsePattern parseExpr tokens = do
  (firstPattern, afterFirst) <- parseSinglePattern tokens
  (orPattern, afterOr) <- parseOrTail firstPattern afterFirst
  parseAsTail orPattern afterOr
  where
    parseOrTail left (Token PipeToken _ pos' : rest) = do
      (right, afterRight) <- parseSinglePattern rest
      let merged =
            case left of
              OrPattern patterns _ -> patterns ++ [right]
              _ -> [left, right]
      parseOrTail (OrPattern merged pos') afterRight
    parseOrTail left rest = Right (left, rest)

    parseAsTail patternValue (Token AsToken _ asPos : Token IdentifierToken aliasName _ : rest) =
      Right (AsPattern patternValue aliasName asPos, rest)
    parseAsTail patternValue rest = Right (patternValue, rest)

    parseSinglePattern (Token IdentifierToken "_" pos' : rest) = Right (WildcardPattern pos', rest)
    parseSinglePattern (Token IdentifierToken name pos' : rest) = Right (CapturePattern name pos', rest)
    parseSinglePattern (Token LBracketToken _ pos' : rest) = parseSequencePattern pos' rest
    parseSinglePattern (Token LBraceToken _ pos' : rest) = parseMappingPattern pos' rest
    parseSinglePattern ts@(Token IntegerToken _ _ : _) = parseValuePattern ts
    parseSinglePattern ts@(Token FloatToken _ _ : _) = parseValuePattern ts
    parseSinglePattern ts@(Token StringToken _ _ : _) = parseValuePattern ts
    parseSinglePattern ts@(Token NoneToken _ _ : _) = parseValuePattern ts
    parseSinglePattern ts@(Token TrueToken _ _ : _) = parseValuePattern ts
    parseSinglePattern ts@(Token FalseToken _ _ : _) = parseValuePattern ts
    parseSinglePattern (tok : _) = Left (ExpectedExpression (position tok))
    parseSinglePattern _ = Left (ExpectedExpression (Position 0 0))

    parseValuePattern ts = do
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
    parseSequencePattern seqPos ts = parseSequenceItems [] Nothing ts
      where
        parseSequenceItems acc restCapture (Token RBracketToken _ _ : rest) =
          Right (SequencePattern acc restCapture seqPos, rest)
        parseSequenceItems acc Nothing (Token StarToken _ _ : Token IdentifierToken "_" _ : afterRest) =
          parseAfterStar acc Nothing afterRest
        parseSequenceItems acc Nothing (Token StarToken _ _ : Token IdentifierToken name _ : afterRest) =
          parseAfterStar acc (Just name) afterRest
        parseSequenceItems acc restCapture stream = do
          (itemPattern, afterItem) <- parseSinglePattern stream
          case afterItem of
            Token CommaToken _ _ : rest -> parseSequenceItems (acc ++ [itemPattern]) restCapture rest
            Token RBracketToken _ _ : rest -> Right (SequencePattern (acc ++ [itemPattern]) restCapture seqPos, rest)
            tok : _ -> Left (ExpectedExpression (position tok))
            _ -> Left (ExpectedExpression (Position 0 0))

        parseAfterStar acc restCapture (Token CommaToken _ _ : Token RBracketToken _ _ : rest) =
          Right (SequencePattern acc restCapture seqPos, rest)
        parseAfterStar acc restCapture (Token RBracketToken _ _ : rest) =
          Right (SequencePattern acc restCapture seqPos, rest)
        parseAfterStar _ _ (tok : _) = Left (ExpectedExpression (position tok))
        parseAfterStar _ _ _ = Left (ExpectedExpression (Position 0 0))

    parseMappingPattern mapPos (Token RBraceToken _ _ : rest) =
      Right (MappingPattern [] Nothing mapPos, rest)
    parseMappingPattern mapPos ts = parseEntries [] Nothing ts
      where
        parseEntries acc maybeRestCapture stream =
          case stream of
            Token StarToken _ _ : Token StarToken _ _ : Token IdentifierToken restName _ : afterRest ->
              parseAfterDoubleStar (acc, Just restName) afterRest
            _ -> do
              (keyExpr, afterKey) <- parseExpr stream
              case afterKey of
                Token ColonToken _ _ : afterColon -> do
                  (valuePattern, afterValue) <- parseSinglePattern afterColon
                  case afterValue of
                    Token CommaToken _ _ : rest -> parseEntries (acc ++ [(keyExpr, valuePattern)]) maybeRestCapture rest
                    Token RBraceToken _ _ : rest -> Right (MappingPattern (acc ++ [(keyExpr, valuePattern)]) maybeRestCapture mapPos, rest)
                    tok : _ -> Left (ExpectedExpression (position tok))
                    _ -> Left (ExpectedExpression (Position 0 0))
                tok : _ -> Left (ExpectedExpression (position tok))
                _ -> Left (ExpectedExpression (Position 0 0))

        parseAfterDoubleStar (acc, maybeRestCapture) afterRest =
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
