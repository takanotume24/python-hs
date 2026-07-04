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
        ClassToken,
        ColonToken,
        CommaToken,
        ContinueToken,
        DefToken,
        DotToken,
        DoubleSlashAssignToken,
        FinallyToken,
        ForToken,
        FromToken,
        GlobalToken,
        GtToken,
        IdentifierToken,
        IfToken,
        ImportToken,
        InToken,
        LParenToken,
        MatchToken,
        MinusAssignToken,
        MinusToken,
        NewlineToken,
        PassToken,
        PercentAssignToken,
        PlusAssignToken,
        PrintToken,
        RaiseToken,
        ReturnToken,
        SlashAssignToken,
        StarAssignToken,
        TryToken,
        WhileToken,
        WithToken,
        YieldToken
      ),
  )
import PythonHS.Parser.DropLeadingNewlines (dropLeadingNewlines)
import PythonHS.Parser.ParseAnnAssignStmt (parseAnnAssignStmt)
import PythonHS.Parser.ParseAnnAssignStmtConfig (ParseAnnAssignStmtConfig (..))
import PythonHS.Parser.ParseClassStmt (parseClassStmt)
import PythonHS.Parser.ParseClassStmtConfig (ParseClassStmtConfig (..))
import PythonHS.Parser.ParseDecoratedStmt (parseDecoratedStmt)
import PythonHS.Parser.ParseDecoratedStmtConfig (ParseDecoratedStmtConfig (..))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseExceptSuites (parseExceptSuites)
import PythonHS.Parser.ParseExceptSuitesConfig (ParseExceptSuitesConfig (..))
import PythonHS.Parser.ParseExpr (parseExpr)
import PythonHS.Parser.ParseIfTail (parseIfTail)
import PythonHS.Parser.ParseIfTailConfig (ParseIfTailConfig (..))
import PythonHS.Parser.ParseImportStmt (parseImportStmt)
import PythonHS.Parser.ParseMatchStmt (parseMatchStmt)
import PythonHS.Parser.ParseMatchStmtConfig (ParseMatchStmtConfig (..))
import PythonHS.Parser.ParseParameters (parseParameters)
import PythonHS.Parser.ParseSuite (parseSuite)
import PythonHS.Parser.ParseUnpackAssign (parseUnpackAssign)
import PythonHS.Parser.ParseUnpackAssignConfig (ParseUnpackAssignConfig (..))
import PythonHS.Parser.ParseWithStmt (parseWithStmt)
import PythonHS.Parser.ParseWithStmtConfig (ParseWithStmtConfig (..))
import PythonHS.Parser.ParseYieldStmt (parseYieldStmt)
import PythonHS.Parser.ParseYieldStmtConfig (ParseYieldStmtConfig (..))

parseStatement :: [Token] -> Either ParseError (Stmt, [Token])
parseStatement tokenStream =
  let parseSuiteWithStatements = parseSuite parseStatement
   in case tokenStream of
        Token AtToken _ pos : _ ->
          parseDecoratedStmt (ParseDecoratedStmtConfig {parseDecoratedStmtExpr = parseExpr, parseDecoratedStmtStatement = parseStatement, parseDecoratedStmtPos = pos}) tokenStream
        Token PrintToken _ pos : rest -> do
          (valueExpr, remaining) <- parseExpr rest
          Right (PrintStmt valueExpr pos, remaining)
        Token ReturnToken _ pos : rest@(Token NewlineToken _ _ : _) ->
          Right (ReturnStmt (NoneExpr pos) pos, rest)
        Token ReturnToken _ pos : rest -> do
          (valueExpr, remaining) <- parseExpr rest
          Right (ReturnStmt valueExpr pos, remaining)
        Token YieldToken _ pos : rest ->
          parseYieldStmt (ParseYieldStmtConfig {parseYieldStmtExpr = parseExpr, parseYieldStmtPos = pos}) rest
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
        Token IdentifierToken firstName pos : Token CommaToken _ _ : rest ->
          parseUnpackAssign (ParseUnpackAssignConfig {parseUnpackAssignFirstName = firstName, parseUnpackAssignPos = pos}) rest
        Token IdentifierToken name pos : Token ColonToken _ _ : rest ->
          parseAnnAssignStmt (ParseAnnAssignStmtConfig {parseAnnAssignStmtExpr = parseExpr, parseAnnAssignStmtName = name, parseAnnAssignStmtPos = pos}) rest
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
              (elseBranch, finalRest) <- parseIfTail (ParseIfTailConfig {parseIfTailSuite = parseSuiteWithStatements}) afterThen
              Right (IfStmt cond thenSuite elseBranch pos, finalRest)
            Token _ _ pos' : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position 0 0})
        Token TryToken _ pos : rest ->
          case rest of
            Token ColonToken _ _ : afterColon -> do
              (trySuite, afterTrySuite) <- parseSuiteWithStatements afterColon
              case parseExceptSuites (ParseExceptSuitesConfig {parseExceptSuitesSuite = parseSuiteWithStatements}) (dropLeadingNewlines afterTrySuite) of
                Right (exceptSuites, afterExceptSuites) ->
                  case dropLeadingNewlines afterExceptSuites of
                    Token FinallyToken _ _ : Token ColonToken _ _ : afterFinallyColon -> do
                      (finallySuite, finalRest) <- parseSuiteWithStatements afterFinallyColon
                      Right (TryExceptStmt trySuite exceptSuites (Just finallySuite) pos, finalRest)
                    _ -> Right (TryExceptStmt trySuite exceptSuites Nothing pos, afterExceptSuites)
                Left err -> Left err
            Token _ _ pos' : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position 0 0})
        Token MatchToken _ pos : rest ->
          parseMatchStmt (ParseMatchStmtConfig {parseMatchStmtExpr = parseExpr, parseMatchStmtSuite = parseSuiteWithStatements}) pos rest
        Token WhileToken _ pos : rest -> do
          (cond, afterCond) <- parseExpr rest
          case afterCond of
            Token ColonToken _ _ : afterColon -> do
              (bodySuite, finalRest) <- parseSuiteWithStatements afterColon
              Right (WhileStmt cond bodySuite pos, finalRest)
            Token _ _ pos' : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position 0 0})
        Token ForToken _ pos : Token IdentifierToken name _ : Token InToken _ _ : rest -> do
          (iterExpr, afterIter) <- parseExpr rest
          case afterIter of
            Token ColonToken _ _ : afterColon -> do
              (bodySuite, finalRest) <- parseSuiteWithStatements afterColon
              Right (ForStmt name iterExpr bodySuite pos, finalRest)
            Token _ _ pos' : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position 0 0})
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
                Token _ _ pos' : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
                _ -> Left (ExpectedExpression {parseErrorPosition = Position 0 0})
            Token ColonToken _ _ : afterColon -> do
              (bodySuite, finalRest) <- parseSuiteWithStatements afterColon
              if null defaults
                then Right (FunctionDefStmt name params bodySuite posDef, finalRest)
                else Right (FunctionDefDefaultsStmt name params defaults bodySuite posDef, finalRest)
            Token _ _ pos' : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position 0 0})
        Token WithToken _ pos : rest ->
          parseWithStmt (ParseWithStmtConfig {parseWithStmtStatement = parseStatement, parseWithStmtPos = pos}) rest
        Token ClassToken _ posClass : Token IdentifierToken name _ : rest ->
          parseClassStmt (ParseClassStmtConfig {parseClassStmtSuite = parseSuiteWithStatements, parseClassStmtPos = posClass, parseClassStmtName = name}) rest
        Token IdentifierToken _ pos : _ -> Left (ExpectedAssignAfterIdentifier {parseErrorPosition = pos})
        tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
        [] -> Left (ExpectedExpression {parseErrorPosition = Position 0 0})
