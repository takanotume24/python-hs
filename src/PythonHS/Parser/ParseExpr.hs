module PythonHS.Parser.ParseExpr (parseExpr) where
import PythonHS.AST.BinaryOperator (BinaryOperator (AddOperator, AndOperator, DivideOperator, EqOperator, FloorDivideOperator, GtOperator, GteOperator, LtOperator, LteOperator, ModuloOperator, MultiplyOperator, NotEqOperator, OrOperator, SubtractOperator))
import PythonHS.AST.Expr (Expr (BinaryExpr, CallExpr, DictExpr, FloatExpr, IdentifierExpr, IntegerExpr, KeywordArgExpr, ListExpr, NoneExpr, NotExpr, StringExpr, UnaryMinusExpr))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (AndToken, AssignToken, ColonToken, CommaToken, DotToken, DoubleSlashToken, EqToken, FalseToken, FloatToken, GtToken, GteToken, IdentifierToken, IntegerToken, LBraceToken, LBracketToken, LParenToken, LtToken, LteToken, MinusToken, NoneToken, NotEqToken, NotToken, OrToken, PercentToken, PlusToken, RBraceToken, RBracketToken, RParenToken, SlashToken, StarToken, StringToken, TrueToken))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))

parseExpr :: [Token] -> Either ParseError (Expr, [Token])
parseExpr = parseOr
  where
    parseOr ts = do
      (left, rest) <- parseAnd ts
      parseOrTail left rest

    parseOrTail left (Token OrToken _ pos : rest) = do
      (right, afterRight) <- parseAnd rest
      parseOrTail (BinaryExpr OrOperator left right pos) afterRight
    parseOrTail left rest = Right (left, rest)

    parseAnd ts = do
      (left, rest) <- parseNot ts
      parseAndTail left rest

    parseAndTail left (Token AndToken _ pos : rest) = do
      (right, afterRight) <- parseNot rest
      parseAndTail (BinaryExpr AndOperator left right pos) afterRight
    parseAndTail left rest = Right (left, rest)

    parseNot (Token NotToken _ pos : rest) = do
      (expr, remaining) <- parseNot rest
      Right (NotExpr expr pos, remaining)
    parseNot ts = parseComparison ts

    parseComparison ts = do
      (left, rest) <- parseAdd ts
      parseComparisonTail left rest

    parseComparisonTail left (Token EqToken _ pos : rest) = do
      (right, afterRight) <- parseAdd rest
      parseComparisonTail (BinaryExpr EqOperator left right pos) afterRight
    parseComparisonTail left (Token NotEqToken _ pos : rest) = do
      (right, afterRight) <- parseAdd rest
      parseComparisonTail (BinaryExpr NotEqOperator left right pos) afterRight
    parseComparisonTail left (Token LtToken _ pos : rest) = do
      (right, afterRight) <- parseAdd rest
      parseComparisonTail (BinaryExpr LtOperator left right pos) afterRight
    parseComparisonTail left (Token GtToken _ pos : rest) = do
      (right, afterRight) <- parseAdd rest
      parseComparisonTail (BinaryExpr GtOperator left right pos) afterRight
    parseComparisonTail left (Token LteToken _ pos : rest) = do
      (right, afterRight) <- parseAdd rest
      parseComparisonTail (BinaryExpr LteOperator left right pos) afterRight
    parseComparisonTail left (Token GteToken _ pos : rest) = do
      (right, afterRight) <- parseAdd rest
      parseComparisonTail (BinaryExpr GteOperator left right pos) afterRight
    parseComparisonTail left remaining = Right (left, remaining)

    parseAdd tokenStream = do
      (leftExpr, remaining) <- parseMul tokenStream
      parseAddTail leftExpr remaining

    parseAddTail left (Token PlusToken _ pos : rest) = do
      (rightExpr, remaining) <- parseMul rest
      parseAddTail (BinaryExpr AddOperator left rightExpr pos) remaining
    parseAddTail left (Token MinusToken _ pos : rest) = do
      (rightExpr, remaining) <- parseMul rest
      parseAddTail (BinaryExpr SubtractOperator left rightExpr pos) remaining
    parseAddTail left remaining = Right (left, remaining)

    parseMul tokenStream = do
      (leftExpr, remaining) <- parsePrimary tokenStream
      parseMulTail leftExpr remaining

    parseMulTail left (Token StarToken _ pos : rest) = do
      (rightExpr, remaining) <- parsePrimary rest
      parseMulTail (BinaryExpr MultiplyOperator left rightExpr pos) remaining
    parseMulTail left (Token SlashToken _ pos : rest) = do
      (rightExpr, remaining) <- parsePrimary rest
      parseMulTail (BinaryExpr DivideOperator left rightExpr pos) remaining
    parseMulTail left (Token DoubleSlashToken _ pos : rest) = do
      (rightExpr, remaining) <- parsePrimary rest
      parseMulTail (BinaryExpr FloorDivideOperator left rightExpr pos) remaining
    parseMulTail left (Token PercentToken _ pos : rest) = do
      (rightExpr, remaining) <- parsePrimary rest
      parseMulTail (BinaryExpr ModuloOperator left rightExpr pos) remaining
    parseMulTail left remaining = Right (left, remaining)

    parsePrimary tokenStream = do
      (baseExpr, remaining) <- parseAtom tokenStream
      parsePostfix baseExpr remaining

    parseAtom (Token IntegerToken value pos : rest) = Right (IntegerExpr (read value) pos, rest)
    parseAtom (Token FloatToken value pos : rest) = Right (FloatExpr (read (normalizeFloatLiteral value)) pos, rest)
    parseAtom (Token TrueToken _ pos : rest) = Right (IntegerExpr 1 pos, rest)
    parseAtom (Token FalseToken _ pos : rest) = Right (IntegerExpr 0 pos, rest)
    parseAtom (Token NoneToken _ pos : rest) = Right (NoneExpr pos, rest)
    parseAtom (Token MinusToken _ pos : Token IntegerToken value _ : rest) = Right (IntegerExpr (negate (read value)) pos, rest)
    parseAtom (Token MinusToken _ pos : Token FloatToken value _ : rest) = Right (FloatExpr (negate (read (normalizeFloatLiteral value))) pos, rest)
    parseAtom (Token MinusToken _ pos : rest) = do
      (expr, remaining) <- parsePrimary rest
      Right (UnaryMinusExpr expr pos, remaining)
    parseAtom (Token StringToken value pos : rest) = Right (StringExpr value pos, rest)
    parseAtom (Token LBracketToken _ pos : rest) = parseListElements pos rest
    parseAtom (Token LBraceToken _ pos : rest) = parseDictEntries pos rest
    parseAtom (Token LParenToken _ _ : rest) = do
      (expr, afterExpr) <- parseExpr rest
      case afterExpr of
        Token RParenToken _ _ : rest' -> Right (expr, rest')
        Token _ _ pos : _ -> Left (ExpectedExpression pos)
        _ -> Left (ExpectedExpression (Position 0 0))
    parseAtom (Token IdentifierToken value pos : rest) = Right (IdentifierExpr value pos, rest)
    parseAtom (tok : _) = Left (ExpectedExpression (position tok))
    parseAtom _ = Left (ExpectedExpression (Position 0 0))

    parsePostfix (IdentifierExpr name pos) (Token LParenToken _ _ : rest) = do
      (args, afterArgs) <- parseArguments rest
      parsePostfix (CallExpr name args pos) afterArgs
    parsePostfix receiverExpr (Token DotToken _ _ : Token IdentifierToken methodName methodPos : Token LParenToken _ _ : rest) = do
      (args, afterArgs) <- parseArguments rest
      parsePostfix (CallExpr methodName (receiverExpr : args) methodPos) afterArgs
    parsePostfix expr rest = Right (expr, rest)
    parseListElements listPos (Token RBracketToken _ _ : rest) =
      Right (ListExpr [] listPos, rest)
    parseListElements listPos ts = do
      (firstExpr, afterFirst) <- parseExpr ts
      parseListTail listPos [firstExpr] afterFirst

    parseListTail listPos exprs (Token CommaToken _ _ : rest) = do
      case rest of
        Token RBracketToken _ _ : afterBracket -> Right (ListExpr exprs listPos, afterBracket)
        _ -> do
          (nextExpr, afterNext) <- parseExpr rest
          parseListTail listPos (exprs ++ [nextExpr]) afterNext
    parseListTail listPos exprs (Token RBracketToken _ _ : rest) =
      Right (ListExpr exprs listPos, rest)
    parseListTail _ _ (tok : _) = Left (ExpectedExpression (position tok))
    parseListTail _ _ _ = Left (ExpectedExpression (Position 0 0))
    parseDictEntries dictPos (Token RBraceToken _ _ : rest) =
      Right (DictExpr [] dictPos, rest)
    parseDictEntries dictPos ts = do
      (keyExpr, afterKey) <- parseExpr ts
      case afterKey of
        Token ColonToken _ _ : afterColon -> do
          (valueExpr, afterValue) <- parseExpr afterColon
          parseDictTail dictPos [(keyExpr, valueExpr)] afterValue
        tok : _ -> Left (ExpectedExpression (position tok))
        _ -> Left (ExpectedExpression (Position 0 0))
    parseDictTail dictPos pairs (Token CommaToken _ _ : rest) = do
      case rest of
        Token RBraceToken _ _ : afterBrace -> Right (DictExpr pairs dictPos, afterBrace)
        _ -> do
          (nextKey, afterKey) <- parseExpr rest
          case afterKey of
            Token ColonToken _ _ : afterColon -> do
              (nextValue, afterValue) <- parseExpr afterColon
              parseDictTail dictPos (pairs ++ [(nextKey, nextValue)]) afterValue
            tok : _ -> Left (ExpectedExpression (position tok))
            _ -> Left (ExpectedExpression (Position 0 0))
    parseDictTail dictPos pairs (Token RBraceToken _ _ : rest) =
      Right (DictExpr pairs dictPos, rest)
    parseDictTail _ _ (tok : _) = Left (ExpectedExpression (position tok))
    parseDictTail _ _ _ = Left (ExpectedExpression (Position 0 0))
    parseArguments (Token RParenToken _ _ : rest) = Right ([], rest)
    parseArguments ts = parseArgumentsTail False [] ts
    parseArgumentsTail seenKeywordArg accArgs tokenStream = do
      (argExpr, isKeywordArg, mismatchPos, afterArg) <- parseCallArgument tokenStream
      if seenKeywordArg && not isKeywordArg
        then Left (ExpectedExpression mismatchPos)
        else
          case afterArg of
            Token RParenToken _ _ : rest -> Right (accArgs ++ [argExpr], rest)
            Token CommaToken _ _ : rest ->
              case rest of
                Token RParenToken _ _ : afterParen -> Right (accArgs ++ [argExpr], afterParen)
                _ -> parseArgumentsTail (seenKeywordArg || isKeywordArg) (accArgs ++ [argExpr]) rest
            Token _ _ pos : _ -> Left (ExpectedExpression pos)
            _ -> Left (ExpectedExpression (Position 0 0))
    parseCallArgument (Token IdentifierToken name namePos : Token AssignToken _ assignPos : rest) = do
      (valueExpr, afterValue) <- parseExpr rest
      Right (KeywordArgExpr name valueExpr namePos, True, assignPos, afterValue)
    parseCallArgument tokenStream = do
      (argExpr, afterArg) <- parseExpr tokenStream
      Right (argExpr, False, exprPos argExpr, afterArg)
    normalizeFloatLiteral literal =
      let withLeading = if take 1 literal == "." then '0' : literal else literal
       in if not (null withLeading) && last withLeading == '.' then withLeading ++ "0" else withLeading

    exprPos (IntegerExpr _ pos) = pos
    exprPos (FloatExpr _ pos) = pos
    exprPos (StringExpr _ pos) = pos
    exprPos (NoneExpr pos) = pos
    exprPos (ListExpr _ pos) = pos
    exprPos (DictExpr _ pos) = pos
    exprPos (IdentifierExpr _ pos) = pos
    exprPos (KeywordArgExpr _ _ pos) = pos
    exprPos (UnaryMinusExpr _ pos) = pos
    exprPos (NotExpr _ pos) = pos
    exprPos (BinaryExpr _ _ _ pos) = pos
    exprPos (CallExpr _ _ pos) = pos