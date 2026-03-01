module PythonHS.Parser.ParseStatement (parseStatement) where
import qualified Data.Set as Set
import PythonHS.AST.Expr (Expr (NoneExpr))
import PythonHS.AST.Stmt (Stmt (AddAssignStmt, AssignStmt, BreakStmt, ContinueStmt, DivAssignStmt, FloorDivAssignStmt, ForStmt, FunctionDefDefaultsStmt, FunctionDefStmt, GlobalStmt, IfStmt, ImportStmt, ModAssignStmt, MulAssignStmt, PassStmt, PrintStmt, ReturnStmt, SubAssignStmt, WhileStmt))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType
  ( TokenType
      ( AssignToken,
        BreakToken,
        ColonToken,
        CommaToken,
        ContinueToken,
        DedentToken,
        DefToken,
        DoubleSlashAssignToken,
        ForToken,
        GlobalToken,
        IdentifierToken,
        ImportToken,
        IfToken,
        InToken,
        IndentToken,
        LParenToken,
        MinusAssignToken,
        NewlineToken,
        PassToken,
        PercentAssignToken,
        PlusAssignToken,
        PrintToken,
        RParenToken,
        ReturnToken,
        SlashAssignToken,
        StarAssignToken,
        WhileToken
      )
  )
import PythonHS.Parser.ParseError (ParseError (ExpectedAssignAfterIdentifier, ExpectedExpression, ExpectedNewlineAfterStatement))
import PythonHS.Parser.ParseExpr (parseExpr)
import PythonHS.Parser.ParseIfTail (parseIfTail)
parseStatement :: [Token] -> Either ParseError (Stmt, [Token])
parseStatement tokenStream =
  case tokenStream of
    Token PrintToken _ pos : rest -> do
      (valueExpr, remaining) <- parseExpr rest
      Right (PrintStmt valueExpr pos, remaining)
    Token ReturnToken _ pos : rest@(Token NewlineToken _ _ : _) ->
      Right (ReturnStmt (NoneExpr pos) pos, rest)
    Token ReturnToken _ pos : rest -> do
      (valueExpr, remaining) <- parseExpr rest
      Right (ReturnStmt valueExpr pos, remaining)
    Token BreakToken _ pos : rest -> Right (BreakStmt pos, rest)
    Token ContinueToken _ pos : rest -> Right (ContinueStmt pos, rest)
    Token PassToken _ pos : rest -> Right (PassStmt pos, rest)
    Token GlobalToken _ pos : Token IdentifierToken name _ : rest ->
      Right (GlobalStmt name pos, rest)
    Token ImportToken _ pos : Token IdentifierToken name _ : rest ->
      Right (ImportStmt name pos, rest)
    Token IdentifierToken name pos : Token AssignToken _ _ : rest -> do
      (valueExpr, remaining) <- parseExpr rest
      Right (AssignStmt name valueExpr pos, remaining)
    Token IdentifierToken name pos : Token PlusAssignToken _ _ : rest -> do
      (valueExpr, remaining) <- parseExpr rest
      Right (AddAssignStmt name valueExpr pos, remaining)
    Token IdentifierToken name pos : Token MinusAssignToken _ _ : rest -> do
      (valueExpr, remaining) <- parseExpr rest
      Right (SubAssignStmt name valueExpr pos, remaining)
    Token IdentifierToken name pos : Token StarAssignToken _ _ : rest -> do
      (valueExpr, remaining) <- parseExpr rest
      Right (MulAssignStmt name valueExpr pos, remaining)
    Token IdentifierToken name pos : Token SlashAssignToken _ _ : rest -> do
      (valueExpr, remaining) <- parseExpr rest
      Right (DivAssignStmt name valueExpr pos, remaining)
    Token IdentifierToken name pos : Token PercentAssignToken _ _ : rest -> do
      (valueExpr, remaining) <- parseExpr rest
      Right (ModAssignStmt name valueExpr pos, remaining)
    Token IdentifierToken name pos : Token DoubleSlashAssignToken _ _ : rest -> do
      (valueExpr, remaining) <- parseExpr rest
      Right (FloorDivAssignStmt name valueExpr pos, remaining)
    Token IfToken _ pos : rest -> do
      (cond, afterCond) <- parseExpr rest
      case afterCond of
        Token ColonToken _ _ : afterColon -> do
          (thenSuite, afterThen) <- parseSuite afterColon
          (elseBranch, finalRest) <- parseIfTail parseSuite afterThen
          Right (IfStmt cond thenSuite elseBranch pos, finalRest)
        Token _ _ pos' : _ -> Left (ExpectedExpression pos')
        _ -> Left (ExpectedExpression (Position 0 0))
    Token WhileToken _ pos : rest -> do
      (cond, afterCond) <- parseExpr rest
      case afterCond of
        Token ColonToken _ _ : afterColon -> do
          (bodySuite, finalRest) <- parseSuite afterColon
          Right (WhileStmt cond bodySuite pos, finalRest)
        Token _ _ pos' : _ -> Left (ExpectedExpression pos')
        _ -> Left (ExpectedExpression (Position 0 0))
    Token ForToken _ pos : Token IdentifierToken name _ : Token InToken _ _ : rest -> do
      (iterExpr, afterIter) <- parseExpr rest
      case afterIter of
        Token ColonToken _ _ : afterColon -> do
          (bodySuite, finalRest) <- parseSuite afterColon
          Right (ForStmt name iterExpr bodySuite pos, finalRest)
        Token _ _ pos' : _ -> Left (ExpectedExpression pos')
        _ -> Left (ExpectedExpression (Position 0 0))
    Token DefToken _ posDef : Token IdentifierToken name _ : Token LParenToken _ _ : rest -> do
      (params, defaults, afterParams) <- parseParameters rest
      case afterParams of
        Token ColonToken _ _ : afterColon -> do
          (bodySuite, finalRest) <- parseSuite afterColon
          if null defaults
            then Right (FunctionDefStmt name params bodySuite posDef, finalRest)
            else Right (FunctionDefDefaultsStmt name params defaults bodySuite posDef, finalRest)
        Token _ _ pos' : _ -> Left (ExpectedExpression pos')
        _ -> Left (ExpectedExpression (Position 0 0))
    Token IdentifierToken _ pos : _ -> Left (ExpectedAssignAfterIdentifier pos)
    tok : _ -> Left (ExpectedExpression (position tok))
    [] -> Left (ExpectedExpression (Position 0 0))
  where
    parseSuite ts =
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
    parseIndentedSuite ts = do
      (statement, restAfterStatement) <- parseStatement ts
      restAfterNewline <- consumeNewline restAfterStatement
      case restAfterNewline of
        Token DedentToken _ dedentPos : rest ->
          Right ([statement], Token NewlineToken "\\n" dedentPos : rest)
        _ -> do
          (otherStatements, finalRest) <- parseIndentedSuite restAfterNewline
          Right (statement : otherStatements, finalRest)

    consumeNewline (Token NewlineToken _ _ : rest) = Right rest
    consumeNewline (Token _ _ pos : _) = Left (ExpectedNewlineAfterStatement pos)
    consumeNewline [] = Left (ExpectedNewlineAfterStatement (Position 0 0))

    parseParameters ts = parseParametersWithState False Set.empty ts
    parseParametersWithState _ _ (Token RParenToken _ _ : rest) = Right ([], [], rest)
    parseParametersWithState seenDefault seenNames ts = do
      ((paramName, paramDefault, paramPos), afterParam) <- parseSingleParameter ts
      if seenDefault && paramDefault == Nothing
        then Left (ExpectedExpression paramPos)
        else if Set.member paramName seenNames
          then Left (ExpectedExpression paramPos)
        else case afterParam of
          Token RParenToken _ _ : rest ->
            Right ([paramName], paramDefaultPair paramName paramDefault, rest)
          Token CommaToken _ _ : rest -> do
            (otherParams, otherDefaults, after) <- parseParametersWithState (seenDefault || paramDefault /= Nothing) (Set.insert paramName seenNames) rest
            Right (paramName : otherParams, paramDefaultPair paramName paramDefault ++ otherDefaults, after)
          tok : _ -> Left (ExpectedExpression (position tok))
          _ -> Left (ExpectedExpression (Position 0 0))

    parseSingleParameter (Token IdentifierToken p pPos : Token AssignToken _ _ : rest) = do
      (defaultExpr, afterDefault) <- parseExpr rest
      Right ((p, Just defaultExpr, pPos), afterDefault)
    parseSingleParameter (Token IdentifierToken p pPos : rest) = Right ((p, Nothing, pPos), rest)
    parseSingleParameter (tok : _) = Left (ExpectedExpression (position tok))
    parseSingleParameter _ = Left (ExpectedExpression (Position 0 0))
    paramDefaultPair _ Nothing = []
    paramDefaultPair name (Just defaultExpr) = [(name, defaultExpr)]
