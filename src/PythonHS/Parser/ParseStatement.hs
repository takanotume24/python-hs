module PythonHS.Parser.ParseStatement (parseStatement) where
import PythonHS.AST.Expr (Expr (NoneExpr))
import PythonHS.AST.Stmt (Stmt (AddAssignStmt, AssignStmt, BreakStmt, ContinueStmt, DivAssignStmt, FloorDivAssignStmt, ForStmt, FunctionDefDefaultsStmt, FunctionDefStmt, GlobalStmt, IfStmt, ModAssignStmt, MulAssignStmt, PassStmt, PrintStmt, RaiseStmt, ReturnStmt, SubAssignStmt, TryExceptStmt, WhileStmt))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType
  ( TokenType
      ( AssignToken,
        AtToken,
        BreakToken,
        ColonToken,
        ContinueToken,
        ClassToken,
        DefToken,
        DoubleSlashAssignToken,
         DotToken,
         GtToken,
        ForToken,
        FromToken,
        GlobalToken,
        IdentifierToken,
         ImportToken,
         TryToken,
         FinallyToken,
          RaiseToken,
          MatchToken,
          IfToken,
        InToken,
        LParenToken,
         MinusAssignToken,
         MinusToken,
        NewlineToken,
        PassToken,
        PercentAssignToken,
        PlusAssignToken,
        PrintToken,
         ReturnToken,
         SlashAssignToken,
         StarAssignToken,
         WhileToken,
         YieldToken
       )
  )
import PythonHS.Parser.ParseError (ParseError (ExpectedAssignAfterIdentifier, ExpectedExpression))
import PythonHS.Parser.ParseExceptSuites (parseExceptSuites)
import PythonHS.Parser.ParseClassStmt (parseClassStmt)
import PythonHS.Parser.ParseExpr (parseExpr)
import PythonHS.Parser.ParseParameters (parseParameters)
import PythonHS.Parser.ParseIfTail (parseIfTail)
import PythonHS.Parser.ParseImportStmt (parseImportStmt)
import PythonHS.Parser.ParseMatchStmt (parseMatchStmt)
import PythonHS.Parser.ParseDecoratedStmt (parseDecoratedStmt)
import PythonHS.Parser.ParseAnnAssignStmt (parseAnnAssignStmt)
import PythonHS.Parser.ParseSuite (parseSuite)
import PythonHS.Parser.ParseYieldStmt (parseYieldStmt)
parseStatement :: [Token] -> Either ParseError (Stmt, [Token])
parseStatement tokenStream =
  let parseSuiteWithStatements = parseSuite parseStatement
   in
  case tokenStream of
    Token AtToken _ pos : _ ->
      parseDecoratedStmt parseExpr parseStatement pos tokenStream
    Token PrintToken _ pos : rest -> do
      (valueExpr, remaining) <- parseExpr rest
      Right (PrintStmt valueExpr pos, remaining)
    Token ReturnToken _ pos : rest@(Token NewlineToken _ _ : _) ->
      Right (ReturnStmt (NoneExpr pos) pos, rest)
    Token ReturnToken _ pos : rest -> do
      (valueExpr, remaining) <- parseExpr rest
      Right (ReturnStmt valueExpr pos, remaining)
    Token YieldToken _ pos : rest ->
      parseYieldStmt parseExpr pos rest
    Token RaiseToken _ pos : rest -> do
      (valueExpr, remaining) <- parseExpr rest
      Right (RaiseStmt valueExpr pos, remaining)
    Token BreakToken _ pos : rest -> Right (BreakStmt pos, rest)
    Token ContinueToken _ pos : rest -> Right (ContinueStmt pos, rest)
    Token PassToken _ pos : rest -> Right (PassStmt pos, rest)
    Token GlobalToken _ pos : Token IdentifierToken name _ : rest ->
      Right (GlobalStmt name pos, rest)
    Token ImportToken _ _ : _ -> parseImportStmt tokenStream
    Token FromToken _ _ : _ -> parseImportStmt tokenStream
    Token IdentifierToken obj pos : Token DotToken _ _ : Token IdentifierToken attr _ : Token AssignToken _ _ : rest -> do
      (valueExpr, remaining) <- parseExpr rest
      Right (AssignStmt (obj ++ "." ++ attr) valueExpr pos, remaining)
    Token IdentifierToken name pos : Token ColonToken _ _ : rest ->
      parseAnnAssignStmt parseExpr name pos rest
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
          (thenSuite, afterThen) <- parseSuiteWithStatements afterColon
          (elseBranch, finalRest) <- parseIfTail parseSuiteWithStatements afterThen
          Right (IfStmt cond thenSuite elseBranch pos, finalRest)
        Token _ _ pos' : _ -> Left (ExpectedExpression pos')
        _ -> Left (ExpectedExpression (Position 0 0))
    Token TryToken _ pos : rest ->
      case rest of
        Token ColonToken _ _ : afterColon -> do
          (trySuite, afterTrySuite) <- parseSuiteWithStatements afterColon
          case parseExceptSuites parseSuiteWithStatements (dropLeadingNewlines afterTrySuite) of
            Right (exceptSuites, afterExceptSuites) ->
              case dropLeadingNewlines afterExceptSuites of
                Token FinallyToken _ _ : Token ColonToken _ _ : afterFinallyColon -> do
                  (finallySuite, finalRest) <- parseSuiteWithStatements afterFinallyColon
                  Right (TryExceptStmt trySuite exceptSuites (Just finallySuite) pos, finalRest)
                _ -> Right (TryExceptStmt trySuite exceptSuites Nothing pos, afterExceptSuites)
            Left err -> Left err
        Token _ _ pos' : _ -> Left (ExpectedExpression pos')
        _ -> Left (ExpectedExpression (Position 0 0))
    Token MatchToken _ pos : rest ->
      parseMatchStmt parseExpr parseSuiteWithStatements pos rest
    Token WhileToken _ pos : rest -> do
      (cond, afterCond) <- parseExpr rest
      case afterCond of
        Token ColonToken _ _ : afterColon -> do
          (bodySuite, finalRest) <- parseSuiteWithStatements afterColon
          Right (WhileStmt cond bodySuite pos, finalRest)
        Token _ _ pos' : _ -> Left (ExpectedExpression pos')
        _ -> Left (ExpectedExpression (Position 0 0))
    Token ForToken _ pos : Token IdentifierToken name _ : Token InToken _ _ : rest -> do
      (iterExpr, afterIter) <- parseExpr rest
      case afterIter of
        Token ColonToken _ _ : afterColon -> do
          (bodySuite, finalRest) <- parseSuiteWithStatements afterColon
          Right (ForStmt name iterExpr bodySuite pos, finalRest)
        Token _ _ pos' : _ -> Left (ExpectedExpression pos')
        _ -> Left (ExpectedExpression (Position 0 0))
    Token DefToken _ posDef : Token IdentifierToken name _ : Token LParenToken _ _ : rest -> do
      (params, defaults, afterParams) <- parseParameters parseExpr rest
      case afterParams of
        Token MinusToken _ _ : Token GtToken _ _ : afterArrow -> do
          (_, afterAnnotation) <- parseExpr afterArrow
          case afterAnnotation of
            Token ColonToken _ _ : afterColon -> do
              (bodySuite, finalRest) <- parseSuiteWithStatements afterColon
              if null defaults
                then Right (FunctionDefStmt name params bodySuite posDef, finalRest)
                else Right (FunctionDefDefaultsStmt name params defaults bodySuite posDef, finalRest)
            Token _ _ pos' : _ -> Left (ExpectedExpression pos')
            _ -> Left (ExpectedExpression (Position 0 0))
        Token ColonToken _ _ : afterColon -> do
          (bodySuite, finalRest) <- parseSuiteWithStatements afterColon
          if null defaults
            then Right (FunctionDefStmt name params bodySuite posDef, finalRest)
            else Right (FunctionDefDefaultsStmt name params defaults bodySuite posDef, finalRest)
        Token _ _ pos' : _ -> Left (ExpectedExpression pos')
        _ -> Left (ExpectedExpression (Position 0 0))
    Token ClassToken _ posClass : Token IdentifierToken name _ : rest ->
      parseClassStmt parseSuiteWithStatements posClass name rest
    Token IdentifierToken _ pos : _ -> Left (ExpectedAssignAfterIdentifier pos)
    tok : _ -> Left (ExpectedExpression (position tok))
    [] -> Left (ExpectedExpression (Position 0 0))
  where
    dropLeadingNewlines (Token NewlineToken _ _ : rest) = dropLeadingNewlines rest
    dropLeadingNewlines rest = rest
