module PythonHS.Parser.ParseStatement (parseStatement) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (..))
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
import PythonHS.Parser.ParseSuiteConfig (ParseSuiteConfig (..))
import PythonHS.Parser.ParseUnpackAssign (parseUnpackAssign)
import PythonHS.Parser.ParseUnpackAssignConfig (ParseUnpackAssignConfig (..))
import PythonHS.Parser.ParseWithStmt (parseWithStmt)
import PythonHS.Parser.ParseWithStmtConfig (ParseWithStmtConfig (..))
import PythonHS.Parser.ParseYieldStmt (parseYieldStmt)
import PythonHS.Parser.ParseYieldStmtConfig (ParseYieldStmtConfig (..))

parseStatement :: [Token] -> Either ParseError (Stmt, [Token])
parseStatement tokenStream =
  let parseSuiteWithStatements tokens = parseSuite (ParseSuiteConfig {parseSuiteStatement = parseStatement, parseSuiteTokenStream = tokens})
   in case tokenStream of
        Token {tokenType = AtToken, position = pos} : _ ->
          parseDecoratedStmt (ParseDecoratedStmtConfig {parseDecoratedStmtExpr = parseExpr, parseDecoratedStmtStatement = parseStatement, parseDecoratedStmtPos = pos, parseDecoratedStmtTokenStream = tokenStream})
        Token {tokenType = PrintToken, position = pos} : rest -> do
          (valueExpr, remaining) <- parseExpr rest
          Right (PrintStmt {printStmtValue = valueExpr, printStmtPos = pos}, remaining)
        Token {tokenType = ReturnToken, position = pos} : rest@(Token {tokenType = NewlineToken} : _) ->
          Right (ReturnStmt {returnStmtValue = NoneExpr {noneExprPos = pos}, returnStmtPos = pos}, rest)
        Token {tokenType = ReturnToken, position = pos} : rest -> do
          (valueExpr, remaining) <- parseExpr rest
          Right (ReturnStmt {returnStmtValue = valueExpr, returnStmtPos = pos}, remaining)
        Token {tokenType = YieldToken, position = pos} : rest ->
          parseYieldStmt (ParseYieldStmtConfig {parseYieldStmtExpr = parseExpr, parseYieldStmtPos = pos, parseYieldStmtTokenStream = rest})
        Token {tokenType = RaiseToken, position = pos} : rest -> do
          (valueExpr, remaining) <- parseExpr rest
          Right (RaiseStmt {raiseStmtExpr = valueExpr, raiseStmtPos = pos}, remaining)
        Token {tokenType = BreakToken, position = pos} : rest -> Right (BreakStmt {breakStmtPos = pos}, rest)
        Token {tokenType = ContinueToken, position = pos} : rest -> Right (ContinueStmt {continueStmtPos = pos}, rest)
        Token {tokenType = PassToken, position = pos} : rest -> Right (PassStmt {passStmtPos = pos}, rest)
        Token {tokenType = GlobalToken, position = pos} : Token {tokenType = IdentifierToken, lexeme = name} : rest ->
          Right (GlobalStmt {globalStmtName = name, globalStmtPos = pos}, rest)
        Token {tokenType = ImportToken} : _ -> parseImportStmt tokenStream
        Token {tokenType = FromToken} : _ -> parseImportStmt tokenStream
        Token {tokenType = IdentifierToken, lexeme = obj, position = pos} : Token {tokenType = DotToken} : Token {tokenType = IdentifierToken, lexeme = attr} : Token {tokenType = AssignToken} : rest -> do
          (valueExpr, remaining) <- parseExpr rest
          Right (AssignStmt {assignStmtName = obj ++ "." ++ attr, assignStmtValue = valueExpr, assignStmtPos = pos}, remaining)
        Token {tokenType = IdentifierToken, lexeme = firstName, position = pos} : Token {tokenType = CommaToken} : rest ->
          parseUnpackAssign (ParseUnpackAssignConfig {parseUnpackAssignFirstName = firstName, parseUnpackAssignPos = pos, parseUnpackAssignTokenStream = rest})
        Token {tokenType = IdentifierToken, lexeme = name, position = pos} : Token {tokenType = ColonToken} : rest ->
          parseAnnAssignStmt (ParseAnnAssignStmtConfig {parseAnnAssignStmtExpr = parseExpr, parseAnnAssignStmtName = name, parseAnnAssignStmtPos = pos, parseAnnAssignStmtTokenStream = rest})
        Token {tokenType = IdentifierToken, lexeme = name, position = pos} : Token {tokenType = AssignToken} : rest -> do
          (valueExpr, remaining) <- parseExpr rest
          Right (AssignStmt {assignStmtName = name, assignStmtValue = valueExpr, assignStmtPos = pos}, remaining)
        Token {tokenType = IdentifierToken, lexeme = name, position = pos} : Token {tokenType = PlusAssignToken} : rest -> do
          (valueExpr, remaining) <- parseExpr rest
          Right (AddAssignStmt {addAssignStmtName = name, addAssignStmtValue = valueExpr, addAssignStmtPos = pos}, remaining)
        Token {tokenType = IdentifierToken, lexeme = name, position = pos} : Token {tokenType = MinusAssignToken} : rest -> do
          (valueExpr, remaining) <- parseExpr rest
          Right (SubAssignStmt {subAssignStmtName = name, subAssignStmtValue = valueExpr, subAssignStmtPos = pos}, remaining)
        Token {tokenType = IdentifierToken, lexeme = name, position = pos} : Token {tokenType = StarAssignToken} : rest -> do
          (valueExpr, remaining) <- parseExpr rest
          Right (MulAssignStmt {mulAssignStmtName = name, mulAssignStmtValue = valueExpr, mulAssignStmtPos = pos}, remaining)
        Token {tokenType = IdentifierToken, lexeme = name, position = pos} : Token {tokenType = SlashAssignToken} : rest -> do
          (valueExpr, remaining) <- parseExpr rest
          Right (DivAssignStmt {divAssignStmtName = name, divAssignStmtValue = valueExpr, divAssignStmtPos = pos}, remaining)
        Token {tokenType = IdentifierToken, lexeme = name, position = pos} : Token {tokenType = PercentAssignToken} : rest -> do
          (valueExpr, remaining) <- parseExpr rest
          Right (ModAssignStmt {modAssignStmtName = name, modAssignStmtValue = valueExpr, modAssignStmtPos = pos}, remaining)
        Token {tokenType = IdentifierToken, lexeme = name, position = pos} : Token {tokenType = DoubleSlashAssignToken} : rest -> do
          (valueExpr, remaining) <- parseExpr rest
          Right (FloorDivAssignStmt {floorDivAssignStmtName = name, floorDivAssignStmtValue = valueExpr, floorDivAssignStmtPos = pos}, remaining)
        Token {tokenType = IfToken, position = pos} : rest -> do
          (cond, afterCond) <- parseExpr rest
          case afterCond of
            Token {tokenType = ColonToken} : afterColon -> do
              (thenSuite, afterThen) <- parseSuiteWithStatements afterColon
              (elseBranch, finalRest) <- parseIfTail (ParseIfTailConfig {parseIfTailSuite = parseSuiteWithStatements, parseIfTailTokenStream = afterThen})
              Right (IfStmt {ifStmtCond = cond, ifStmtThen = thenSuite, ifStmtElse = elseBranch, ifStmtPos = pos}, finalRest)
            Token {position = pos'} : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
        Token {tokenType = TryToken, position = pos} : rest ->
          case rest of
            Token {tokenType = ColonToken} : afterColon -> do
              (trySuite, afterTrySuite) <- parseSuiteWithStatements afterColon
              let exceptResult = parseExceptSuites (ParseExceptSuitesConfig {parseExceptSuitesSuite = parseSuiteWithStatements, parseExceptSuitesTokenStream = dropLeadingNewlines afterTrySuite})
              case exceptResult of
                Right (exceptSuites, afterExceptSuites) ->
                  case dropLeadingNewlines afterExceptSuites of
                    Token {tokenType = FinallyToken} : Token {tokenType = ColonToken} : afterFinallyColon -> do
                      (finallySuite, finalRest) <- parseSuiteWithStatements afterFinallyColon
                      Right (TryExceptStmt {tryExceptStmtTryBody = trySuite, tryExceptStmtExceptSuites = exceptSuites, tryExceptStmtFinallyBody = Just finallySuite, tryExceptStmtPos = pos}, finalRest)
                    _ -> Right (TryExceptStmt {tryExceptStmtTryBody = trySuite, tryExceptStmtExceptSuites = exceptSuites, tryExceptStmtFinallyBody = Nothing, tryExceptStmtPos = pos}, afterExceptSuites)
                Left err -> Left err
            Token {position = pos'} : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
        Token {tokenType = MatchToken, position = pos} : rest ->
          parseMatchStmt (ParseMatchStmtConfig {parseMatchStmtExpr = parseExpr, parseMatchStmtSuite = parseSuiteWithStatements, parseMatchStmtPos = pos, parseMatchStmtTokenStream = rest})
        Token {tokenType = WhileToken, position = pos} : rest -> do
          (cond, afterCond) <- parseExpr rest
          case afterCond of
            Token {tokenType = ColonToken} : afterColon -> do
              (bodySuite, finalRest) <- parseSuiteWithStatements afterColon
              Right (WhileStmt {whileStmtCond = cond, whileStmtBody = bodySuite, whileStmtPos = pos}, finalRest)
            Token {position = pos'} : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
        Token {tokenType = ForToken, position = pos} : Token {tokenType = IdentifierToken, lexeme = name} : Token {tokenType = InToken} : rest -> do
          (iterExpr, afterIter) <- parseExpr rest
          case afterIter of
            Token {tokenType = ColonToken} : afterColon -> do
              (bodySuite, finalRest) <- parseSuiteWithStatements afterColon
              Right (ForStmt {forStmtVar = name, forStmtIter = iterExpr, forStmtBody = bodySuite, forStmtPos = pos}, finalRest)
            Token {position = pos'} : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
        Token {tokenType = DefToken, position = posDef} : Token {tokenType = IdentifierToken, lexeme = name} : Token {tokenType = LParenToken} : rest -> do
          (params, defaults, afterParams) <- parseParameters parseExpr rest
          case afterParams of
            Token {tokenType = MinusToken} : Token {tokenType = GtToken} : afterArrow -> do
              (_, afterAnnotation) <- parseExpr afterArrow
              case afterAnnotation of
                Token {tokenType = ColonToken} : afterColon -> do
                  (bodySuite, finalRest) <- parseSuiteWithStatements afterColon
                  if null defaults
                    then Right (FunctionDefStmt {functionDefStmtName = name, functionDefStmtParams = params, functionDefStmtBody = bodySuite, functionDefStmtPos = posDef}, finalRest)
                    else Right (FunctionDefDefaultsStmt {functionDefDefaultsStmtName = name, functionDefDefaultsStmtParams = params, functionDefDefaultsStmtDefaults = defaults, functionDefDefaultsStmtBody = bodySuite, functionDefDefaultsStmtPos = posDef}, finalRest)
                Token {position = pos'} : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
                _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
            Token {tokenType = ColonToken} : afterColon -> do
              (bodySuite, finalRest) <- parseSuiteWithStatements afterColon
              if null defaults
                then Right (FunctionDefStmt {functionDefStmtName = name, functionDefStmtParams = params, functionDefStmtBody = bodySuite, functionDefStmtPos = posDef}, finalRest)
                else Right (FunctionDefDefaultsStmt {functionDefDefaultsStmtName = name, functionDefDefaultsStmtParams = params, functionDefDefaultsStmtDefaults = defaults, functionDefDefaultsStmtBody = bodySuite, functionDefDefaultsStmtPos = posDef}, finalRest)
            Token {position = pos'} : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
        Token {tokenType = WithToken, position = pos} : rest ->
          parseWithStmt (ParseWithStmtConfig {parseWithStmtStatement = parseStatement, parseWithStmtPos = pos, parseWithStmtTokenStream = rest})
        Token {tokenType = ClassToken, position = posClass} : Token {tokenType = IdentifierToken, lexeme = name} : rest ->
          parseClassStmt (ParseClassStmtConfig {parseClassStmtSuite = parseSuiteWithStatements, parseClassStmtPos = posClass, parseClassStmtName = name, parseClassStmtTokenStream = rest})
        Token {tokenType = IdentifierToken, position = pos} : _ -> Left (ExpectedAssignAfterIdentifier {parseErrorPosition = pos})
        tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
        [] -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
