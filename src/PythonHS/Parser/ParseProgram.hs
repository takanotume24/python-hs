module PythonHS.Parser.ParseProgram (parseProgram) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AddOperator, MultiplyOperator, DivideOperator, ModuloOperator, EqOperator, NotEqOperator, LtOperator, GtOperator, LteOperator, GteOperator, AndOperator, OrOperator))
import PythonHS.AST.Expr (Expr (BinaryExpr, CallExpr, DictExpr, IdentifierExpr, IntegerExpr, ListExpr, NoneExpr, NotExpr, StringExpr, UnaryMinusExpr))
import PythonHS.AST.Program (Program (Program))
import PythonHS.AST.Stmt (Stmt (AddAssignStmt, AssignStmt, BreakStmt, ContinueStmt, DivAssignStmt, FloorDivAssignStmt, ForStmt, GlobalStmt, IfStmt, FunctionDefStmt, ModAssignStmt, MulAssignStmt, PassStmt, PrintStmt, ReturnStmt, SubAssignStmt, WhileStmt))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.TokenType
  ( TokenType
      ( AssignToken,
        PlusAssignToken,
        MinusAssignToken,
        StarAssignToken,
        SlashAssignToken,
        PercentAssignToken,
        DoubleSlashAssignToken,
        ColonToken,
        CommaToken,
        DefToken,
        EOFToken,
        ElseToken,
        ElifToken,
        TrueToken,
        FalseToken,
        NoneToken,
        IdentifierToken,
        IntegerToken,
        StringToken,
        LParenToken,
        LBracketToken,
        LBraceToken,
        NewlineToken,
        MinusToken,
        PlusToken,
        StarToken,
        SlashToken,
        PercentToken,
        PrintToken,
        RParenToken,
        RBracketToken,
        RBraceToken,
        ReturnToken,
        EqToken,
        GlobalToken,
        PassToken,
        IndentToken,
        DedentToken,
        NotEqToken,
        LtToken,
        GtToken,
        LteToken,
        GteToken,
        AndToken,
        OrToken,
        NotToken,
        IfToken,
        WhileToken,
        ForToken,
        InToken,
        BreakToken,
        ContinueToken
      )
  )
import PythonHS.Parser.ParseError (ParseError (ExpectedAssignAfterIdentifier, ExpectedExpression, ExpectedNewlineAfterStatement))

parseProgram :: [Token] -> Either ParseError Program
parseProgram tokens = fmap (Program . fst) (parseStatements tokens)
  where
    parseStatements [] = Right ([], [])
    parseStatements (Token EOFToken _ _ : rest) = Right ([], rest)
    parseStatements tokenStream = do
      (statement, restAfterStatement) <- parseStatement tokenStream
      restAfterNewline <- consumeNewline restAfterStatement
      (otherStatements, finalRest) <- parseStatements restAfterNewline
      Right (statement : otherStatements, finalRest)

    consumeNewline (Token NewlineToken _ _ : rest) = Right rest
    consumeNewline (Token EOFToken _ pos : rest) = Right (Token EOFToken "" pos : rest)
    consumeNewline (tok : _) = Left (ExpectedNewlineAfterStatement (position tok))
    consumeNewline [] = Left (ExpectedNewlineAfterStatement (Position 0 0))

    -- Statements
    parseStatement (Token PrintToken _ pos : rest) = do
      (valueExpr, remaining) <- parseExpr rest
      Right (PrintStmt valueExpr pos, remaining)

    parseStatement (Token ReturnToken _ pos : rest) = do
      (valueExpr, remaining) <- parseExpr rest
      Right (ReturnStmt valueExpr pos, remaining)

    parseStatement (Token BreakToken _ pos : rest) = Right (BreakStmt pos, rest)
    parseStatement (Token ContinueToken _ pos : rest) = Right (ContinueStmt pos, rest)
    parseStatement (Token PassToken _ pos : rest) = Right (PassStmt pos, rest)

    parseStatement (Token GlobalToken _ pos : Token IdentifierToken name _ : rest) =
      Right (GlobalStmt name pos, rest)

    parseStatement (Token IdentifierToken name pos : Token AssignToken _ _ : rest) = do
      (valueExpr, remaining) <- parseExpr rest
      Right (AssignStmt name valueExpr pos, remaining)

    parseStatement (Token IdentifierToken name pos : Token PlusAssignToken _ _ : rest) = do
      (valueExpr, remaining) <- parseExpr rest
      Right (AddAssignStmt name valueExpr pos, remaining)

    parseStatement (Token IdentifierToken name pos : Token MinusAssignToken _ _ : rest) = do
      (valueExpr, remaining) <- parseExpr rest
      Right (SubAssignStmt name valueExpr pos, remaining)

    parseStatement (Token IdentifierToken name pos : Token StarAssignToken _ _ : rest) = do
      (valueExpr, remaining) <- parseExpr rest
      Right (MulAssignStmt name valueExpr pos, remaining)

    parseStatement (Token IdentifierToken name pos : Token SlashAssignToken _ _ : rest) = do
      (valueExpr, remaining) <- parseExpr rest
      Right (DivAssignStmt name valueExpr pos, remaining)

    parseStatement (Token IdentifierToken name pos : Token PercentAssignToken _ _ : rest) = do
      (valueExpr, remaining) <- parseExpr rest
      Right (ModAssignStmt name valueExpr pos, remaining)

    parseStatement (Token IdentifierToken name pos : Token DoubleSlashAssignToken _ _ : rest) = do
      (valueExpr, remaining) <- parseExpr rest
      Right (FloorDivAssignStmt name valueExpr pos, remaining)

    -- if <cond> : <stmt> [ else : <stmt> ]
    parseStatement (Token IfToken _ pos : rest) = do
      (cond, afterCond) <- parseExpr rest
      case afterCond of
        Token ColonToken _ _ : afterColon -> do
          (thenSuite, afterThen) <- parseSuite afterColon
          (elseBranch, finalRest) <- parseIfTail afterThen
          Right (IfStmt cond thenSuite elseBranch pos, finalRest)
        Token _ _ pos' : _ -> Left (ExpectedExpression pos')
        _ -> Left (ExpectedExpression (Position 0 0))

    -- while <cond> : <stmt>
    parseStatement (Token WhileToken _ pos : rest) = do
      (cond, afterCond) <- parseExpr rest
      case afterCond of
        Token ColonToken _ _ : afterColon -> do
          (bodySuite, finalRest) <- parseSuite afterColon
          Right (WhileStmt cond bodySuite pos, finalRest)
        Token _ _ pos' : _ -> Left (ExpectedExpression pos')
        _ -> Left (ExpectedExpression (Position 0 0))

    -- for <name> in <expr> : <stmt>
    parseStatement (Token ForToken _ pos : Token IdentifierToken name _ : Token InToken _ _ : rest) = do
      (iterExpr, afterIter) <- parseExpr rest
      case afterIter of
        Token ColonToken _ _ : afterColon -> do
          (bodySuite, finalRest) <- parseSuite afterColon
          Right (ForStmt name iterExpr bodySuite pos, finalRest)
        Token _ _ pos' : _ -> Left (ExpectedExpression pos')
        _ -> Left (ExpectedExpression (Position 0 0))

    -- def name(params): <stmt>
    parseStatement (Token DefToken _ posDef : Token IdentifierToken name _ : Token LParenToken _ _ : rest) = do
      (params, afterParams) <- parseParameters rest
      case afterParams of
        Token ColonToken _ _ : afterColon -> do
          (bodySuite, finalRest) <- parseSuite afterColon
          Right (FunctionDefStmt name params bodySuite posDef, finalRest)
        Token _ _ pos' : _ -> Left (ExpectedExpression pos')
        _ -> Left (ExpectedExpression (Position 0 0))

    parseStatement (Token IdentifierToken _ pos : _) = Left (ExpectedAssignAfterIdentifier pos)
    parseStatement (tok : _) = Left (ExpectedExpression (position tok))
    parseStatement [] = Left (ExpectedExpression (Position 0 0))

    -- Parse suite body: either inline single statement, or newline + INDENT ... DEDENT block.
    parseSuite ts = do
      case ts of
        Token NewlineToken _ _ : Token IndentToken _ _ : rest -> parseIndentedSuite rest
        Token NewlineToken _ _ : rest -> do
          (stmt, remaining) <- parseStatement rest
          Right ([stmt], remaining)
        _ -> do
          (stmt, remaining) <- parseStatement ts
          Right ([stmt], remaining)

    parseIndentedSuite (Token DedentToken _ dedentPos : rest) =
      Right ([], Token NewlineToken "\\n" dedentPos : rest)
    parseIndentedSuite tokenStream = do
      (statement, restAfterStatement) <- parseStatement tokenStream
      restAfterNewline <- consumeNewline restAfterStatement
      case restAfterNewline of
        Token DedentToken _ dedentPos : rest ->
          Right ([statement], Token NewlineToken "\\n" dedentPos : rest)
        _ -> do
          (otherStatements, finalRest) <- parseIndentedSuite restAfterNewline
          Right (statement : otherStatements, finalRest)

    -- Parameters: identifier (',' identifier)* ')'
    parseParameters (Token RParenToken _ _ : rest) = Right ([], rest)
    parseParameters (Token IdentifierToken p _ : Token RParenToken _ _ : rest) = Right ([p], rest)
    parseParameters (Token IdentifierToken p _ : Token CommaToken _ _ : rest) = do
      (others, after) <- parseParameters rest
      Right (p : others, after)
    parseParameters (tok : _) = Left (ExpectedExpression (position tok))
    parseParameters _ = Left (ExpectedExpression (Position 0 0))

    dropLeadingNewlines (Token NewlineToken _ _ : rest) = dropLeadingNewlines rest
    dropLeadingNewlines ts = ts

    parseIfTail ts =
      case ts of
        Token ElseToken _ _ : Token ColonToken _ _ : afterElse -> do
          (elseSuite, finalRest) <- parseSuite afterElse
          Right (Just elseSuite, finalRest)
        Token ElifToken _ elifPos : afterElif -> do
          (elifCond, afterElifCond) <- parseExpr afterElif
          case afterElifCond of
            Token ColonToken _ _ : afterElifColon -> do
              (elifThenSuite, afterElifThen) <- parseSuite afterElifColon
              (elifElseBranch, finalRest) <- parseIfTail afterElifThen
              Right (Just [IfStmt elifCond elifThenSuite elifElseBranch elifPos], finalRest)
            Token _ _ pos : _ -> Left (ExpectedExpression pos)
            _ -> Left (ExpectedExpression (Position 0 0))
        Token NewlineToken _ _ : _ ->
          case dropLeadingNewlines ts of
            Token ElseToken _ _ : Token ColonToken _ _ : afterElse -> do
              (elseSuite, finalRest) <- parseSuite afterElse
              Right (Just elseSuite, finalRest)
            Token ElifToken _ elifPos : afterElif -> do
              (elifCond, afterElifCond) <- parseExpr afterElif
              case afterElifCond of
                Token ColonToken _ _ : afterElifColon -> do
                  (elifThenSuite, afterElifThen) <- parseSuite afterElifColon
                  (elifElseBranch, finalRest) <- parseIfTail afterElifThen
                  Right (Just [IfStmt elifCond elifThenSuite elifElseBranch elifPos], finalRest)
                Token _ _ pos : _ -> Left (ExpectedExpression pos)
                _ -> Left (ExpectedExpression (Position 0 0))
            _ -> Right (Nothing, ts)
        _ -> Right (Nothing, ts)

    -- Expressions (precedence: primary -> add -> comparison -> not -> and -> or)
    parseExpr ts = parseOr ts

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
    parseMulTail left (Token PercentToken _ pos : rest) = do
      (rightExpr, remaining) <- parsePrimary rest
      parseMulTail (BinaryExpr ModuloOperator left rightExpr pos) remaining
    parseMulTail left remaining = Right (left, remaining)

    parsePrimary (Token IntegerToken value pos : rest) = Right (IntegerExpr (read value) pos, rest)
    parsePrimary (Token TrueToken _ pos : rest) = Right (IntegerExpr 1 pos, rest)
    parsePrimary (Token FalseToken _ pos : rest) = Right (IntegerExpr 0 pos, rest)
    parsePrimary (Token NoneToken _ pos : rest) = Right (NoneExpr pos, rest)
    parsePrimary (Token MinusToken _ pos : Token IntegerToken value _ : rest) = Right (IntegerExpr (negate (read value)) pos, rest)
    parsePrimary (Token MinusToken _ pos : rest) = do
      (expr, remaining) <- parsePrimary rest
      Right (UnaryMinusExpr expr pos, remaining)
    parsePrimary (Token StringToken value pos : rest) = Right (StringExpr value pos, rest)
    parsePrimary (Token LBracketToken _ pos : rest) = parseListElements pos rest
    parsePrimary (Token LBraceToken _ pos : rest) = parseDictEntries pos rest
    parsePrimary (Token LParenToken _ _ : rest) = do
      (expr, afterExpr) <- parseExpr rest
      case afterExpr of
        Token RParenToken _ _ : rest' -> Right (expr, rest')
        Token _ _ pos : _ -> Left (ExpectedExpression pos)
        _ -> Left (ExpectedExpression (Position 0 0))
    -- function call: identifier '(' args ')'
    parsePrimary (Token IdentifierToken name pos : Token LParenToken _ _ : rest) = do
      (args, afterArgs) <- parseArguments rest
      Right (CallExpr name args pos, afterArgs)
    parsePrimary (Token IdentifierToken value pos : rest) = Right (IdentifierExpr value pos, rest)
    parsePrimary (tok : _) = Left (ExpectedExpression (position tok))
    parsePrimary _ = Left (ExpectedExpression (Position 0 0))

    parseListElements listPos (Token RBracketToken _ _ : rest) =
      Right (ListExpr [] listPos, rest)
    parseListElements listPos ts = do
      (firstExpr, afterFirst) <- parseExpr ts
      parseListTail listPos [firstExpr] afterFirst

    parseListTail listPos exprs (Token CommaToken _ _ : rest) = do
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
        (Token ColonToken _ _ : afterColon) -> do
          (valueExpr, afterValue) <- parseExpr afterColon
          parseDictTail dictPos [(keyExpr, valueExpr)] afterValue
        (tok : _) -> Left (ExpectedExpression (position tok))
        _ -> Left (ExpectedExpression (Position 0 0))

    parseDictTail dictPos pairs (Token CommaToken _ _ : rest) = do
      (nextKey, afterKey) <- parseExpr rest
      case afterKey of
        (Token ColonToken _ _ : afterColon) -> do
          (nextValue, afterValue) <- parseExpr afterColon
          parseDictTail dictPos (pairs ++ [(nextKey, nextValue)]) afterValue
        (tok : _) -> Left (ExpectedExpression (position tok))
        _ -> Left (ExpectedExpression (Position 0 0))
    parseDictTail dictPos pairs (Token RBraceToken _ _ : rest) =
      Right (DictExpr pairs dictPos, rest)
    parseDictTail _ _ (tok : _) = Left (ExpectedExpression (position tok))
    parseDictTail _ _ _ = Left (ExpectedExpression (Position 0 0))

    -- Arguments: expr (',' expr)* ')'
    parseArguments (Token RParenToken _ _ : rest) = Right ([], rest)
    parseArguments ts = do
      (firstArg, afterFirst) <- parseExpr ts
      case afterFirst of
        Token RParenToken _ _ : rest -> Right ([firstArg], rest)
        Token CommaToken _ _ : rest -> do
          (otherArgs, finalRest) <- parseArguments rest
          Right (firstArg : otherArgs, finalRest)
        Token _ _ pos : _ -> Left (ExpectedExpression pos)
        _ -> Left (ExpectedExpression (Position 0 0))
