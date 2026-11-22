module PythonHS.Parser.ParseStatement (parseStatement) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType
  ( TokenType
      ( AssertToken,
        AssignToken,
        AtToken,
        BreakToken,
        ClassToken,
        ColonToken,
        CommaToken,
        ContinueToken,
        DefToken,
        DelToken,
        DotToken,
        ForToken,
        FromToken,
        GlobalToken,
        IdentifierToken,
        IfToken,
        ImportToken,
        MatchToken,
        NewlineToken,
        PassToken,
        PrintToken,
        RaiseToken,
        ReturnToken,
        StringToken,
        TryToken,
        WhileToken,
        WithToken,
        YieldToken
      ),
  )
import PythonHS.Parser.ParseAnnAssignStmt (parseAnnAssignStmt)
import PythonHS.Parser.ParseAnnAssignStmtConfig (ParseAnnAssignStmtConfig (..))
import PythonHS.Parser.ParseAssertStmt (parseAssertStmt)
import PythonHS.Parser.ParseAugAssignStmt (parseAugAssignStmt)
import PythonHS.Parser.ParseAugAssignStmtConfig (ParseAugAssignStmtConfig (..))
import PythonHS.Parser.ParseClassStmt (parseClassStmt)
import PythonHS.Parser.ParseClassStmtConfig (ParseClassStmtConfig (..))
import PythonHS.Parser.ParseCompoundStmt (parseCompoundStmt)
import PythonHS.Parser.ParseCompoundStmtConfig (ParseCompoundStmtConfig (..))
import PythonHS.Parser.ParseDecoratedStmt (parseDecoratedStmt)
import PythonHS.Parser.ParseDecoratedStmtConfig (ParseDecoratedStmtConfig (..))
import PythonHS.Parser.ParseDelStmt (parseDelStmt)
import PythonHS.Parser.ParseDelStmtConfig (ParseDelStmtConfig (..))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseExpr (parseExpr)
import PythonHS.Parser.ParseExprStatement (parseExprStatement)
import PythonHS.Parser.ParseImportStmt (parseImportStmt)
import PythonHS.Parser.ParseMatchStmt (parseMatchStmt)
import PythonHS.Parser.ParseMatchStmtConfig (ParseMatchStmtConfig (..))
import PythonHS.Parser.ParseRaiseStmt (parseRaiseStmt)
import PythonHS.Parser.ParseRaiseStmtConfig (ParseRaiseStmtConfig (..))
import PythonHS.Parser.ParseResult (ParseResult (..))
import PythonHS.Parser.ParseSubscriptAssignStmt (parseSubscriptAssignStmt)
import PythonHS.Parser.ParseSubscriptAssignStmtConfig (ParseSubscriptAssignStmtConfig (..))
import PythonHS.Parser.ParseSuite (parseSuite)
import PythonHS.Parser.ParseSuiteConfig (ParseSuiteConfig (..))
import PythonHS.Parser.ParseUnpackAssign (parseUnpackAssign)
import PythonHS.Parser.ParseUnpackAssignConfig (ParseUnpackAssignConfig (..))
import PythonHS.Parser.ParseWithStmt (parseWithStmt)
import PythonHS.Parser.ParseWithStmtConfig (ParseWithStmtConfig (..))
import PythonHS.Parser.ParseYieldStmt (parseYieldStmt)
import PythonHS.Parser.ParseYieldStmtConfig (ParseYieldStmtConfig (..))

parseStatement :: [Token] -> Either ParseError (ParseResult Stmt)
parseStatement tokenStream =
  let parseSuiteWithStatements tokens = parseSuite (ParseSuiteConfig {parseSuiteStatement = parseStatement, parseSuiteTokenStream = tokens})
      compoundStmtConfig = ParseCompoundStmtConfig {parseCompoundStmtExpr = parseExpr, parseCompoundStmtStatement = parseStatement, parseCompoundStmtTokenStream = tokenStream}
      augAssignConfig = ParseAugAssignStmtConfig {parseAugAssignStmtExpr = parseExpr, parseAugAssignStmtTokenStream = tokenStream}
      subscriptAssignConfig = ParseSubscriptAssignStmtConfig {parseSubscriptAssignStmtExpr = parseExpr, parseSubscriptAssignStmtTokenStream = tokenStream}
   in case tokenStream of
        Token {tokenType = AtToken, position = pos} : _ ->
          parseDecoratedStmt (ParseDecoratedStmtConfig {parseDecoratedStmtExpr = parseExpr, parseDecoratedStmtStatement = parseStatement, parseDecoratedStmtPos = pos, parseDecoratedStmtTokenStream = tokenStream})
        Token {tokenType = AssertToken} : _ -> parseAssertStmt tokenStream
        Token {tokenType = PrintToken, position = pos} : rest -> do
          valueResult <- parseExpr rest
          let valueExpr = parseResultValue valueResult
              remaining = parseResultRemaining valueResult
          Right (ParseResult {parseResultValue = PrintStmt {printStmtValue = valueExpr, printStmtPos = pos}, parseResultRemaining = remaining})
        Token {tokenType = ReturnToken, position = pos} : rest@(Token {tokenType = NewlineToken} : _) ->
          Right (ParseResult {parseResultValue = ReturnStmt {returnStmtValue = NoneExpr {noneExprPos = pos}, returnStmtPos = pos}, parseResultRemaining = rest})
        Token {tokenType = ReturnToken, position = pos} : rest -> do
          valueResult <- parseExpr rest
          let valueExpr = parseResultValue valueResult
              remaining = parseResultRemaining valueResult
          Right (ParseResult {parseResultValue = ReturnStmt {returnStmtValue = valueExpr, returnStmtPos = pos}, parseResultRemaining = remaining})
        Token {tokenType = YieldToken, position = pos} : rest ->
          parseYieldStmt (ParseYieldStmtConfig {parseYieldStmtExpr = parseExpr, parseYieldStmtPos = pos, parseYieldStmtTokenStream = rest})
        Token {tokenType = RaiseToken, position = pos} : rest ->
          parseRaiseStmt (ParseRaiseStmtConfig {parseRaiseStmtExpr = parseExpr, parseRaiseStmtPos = pos, parseRaiseStmtTokenStream = rest})
        Token {tokenType = BreakToken, position = pos} : rest -> Right (ParseResult {parseResultValue = BreakStmt {breakStmtPos = pos}, parseResultRemaining = rest})
        Token {tokenType = ContinueToken, position = pos} : rest -> Right (ParseResult {parseResultValue = ContinueStmt {continueStmtPos = pos}, parseResultRemaining = rest})
        Token {tokenType = PassToken, position = pos} : rest -> Right (ParseResult {parseResultValue = PassStmt {passStmtPos = pos}, parseResultRemaining = rest})
        Token {tokenType = GlobalToken, position = pos} : Token {tokenType = IdentifierToken, lexeme = name} : rest ->
          Right (ParseResult {parseResultValue = GlobalStmt {globalStmtName = name, globalStmtPos = pos}, parseResultRemaining = rest})
        Token {tokenType = DelToken, position = pos} : rest ->
          parseDelStmt (ParseDelStmtConfig {parseDelStmtExpr = parseExpr, parseDelStmtPos = pos, parseDelStmtTokenStream = rest})
        Token {tokenType = ImportToken} : _ -> parseImportStmt tokenStream
        Token {tokenType = FromToken} : _ -> parseImportStmt tokenStream
        Token {tokenType = IdentifierToken, lexeme = obj, position = pos} : Token {tokenType = DotToken} : Token {tokenType = IdentifierToken, lexeme = attr} : Token {tokenType = AssignToken} : rest -> do
          valueResult <- parseExpr rest
          let valueExpr = parseResultValue valueResult
              remaining = parseResultRemaining valueResult
          Right (ParseResult {parseResultValue = AssignStmt {assignStmtName = obj ++ "." ++ attr, assignStmtValue = valueExpr, assignStmtPos = pos}, parseResultRemaining = remaining})
        Token {tokenType = IdentifierToken, lexeme = firstName, position = pos} : Token {tokenType = CommaToken} : rest ->
          parseUnpackAssign (ParseUnpackAssignConfig {parseUnpackAssignFirstName = firstName, parseUnpackAssignPos = pos, parseUnpackAssignTokenStream = rest})
        Token {tokenType = IdentifierToken, lexeme = name, position = pos} : Token {tokenType = ColonToken} : rest ->
          parseAnnAssignStmt (ParseAnnAssignStmtConfig {parseAnnAssignStmtExpr = parseExpr, parseAnnAssignStmtName = name, parseAnnAssignStmtPos = pos, parseAnnAssignStmtTokenStream = rest})
        Token {tokenType = IdentifierToken, lexeme = name, position = pos} : Token {tokenType = AssignToken} : rest -> do
          valueResult <- parseExpr rest
          let valueExpr = parseResultValue valueResult
              remaining = parseResultRemaining valueResult
          Right (ParseResult {parseResultValue = AssignStmt {assignStmtName = name, assignStmtValue = valueExpr, assignStmtPos = pos}, parseResultRemaining = remaining})
        Token {tokenType = IfToken} : _ -> parseCompoundStmt compoundStmtConfig
        Token {tokenType = TryToken} : _ -> parseCompoundStmt compoundStmtConfig
        Token {tokenType = MatchToken, position = pos} : rest ->
          parseMatchStmt (ParseMatchStmtConfig {parseMatchStmtExpr = parseExpr, parseMatchStmtSuite = parseSuiteWithStatements, parseMatchStmtPos = pos, parseMatchStmtTokenStream = rest})
        Token {tokenType = WhileToken} : _ -> parseCompoundStmt compoundStmtConfig
        Token {tokenType = ForToken} : _ -> parseCompoundStmt compoundStmtConfig
        Token {tokenType = DefToken} : _ -> parseCompoundStmt compoundStmtConfig
        Token {tokenType = WithToken, position = pos} : rest ->
          parseWithStmt (ParseWithStmtConfig {parseWithStmtStatement = parseStatement, parseWithStmtPos = pos, parseWithStmtTokenStream = rest})
        Token {tokenType = ClassToken, position = posClass} : Token {tokenType = IdentifierToken, lexeme = name} : rest ->
          parseClassStmt (ParseClassStmtConfig {parseClassStmtSuite = parseSuiteWithStatements, parseClassStmtPos = posClass, parseClassStmtName = name, parseClassStmtTokenStream = rest})
        Token {tokenType = StringToken, position = pos} : _ ->
          case parseExprStatement tokenStream of
            Just statementResult -> Right statementResult
            Nothing -> Left (ExpectedExpression {parseErrorPosition = pos})
        Token {tokenType = IdentifierToken, position = pos} : _ ->
          case parseAugAssignStmt augAssignConfig of
            Just statementResult -> statementResult
            Nothing ->
              case parseSubscriptAssignStmt subscriptAssignConfig of
                Just statementResult -> statementResult
                Nothing ->
                  case parseExprStatement tokenStream of
                    Just statementResult -> Right statementResult
                    Nothing -> Left (ExpectedAssignAfterIdentifier {parseErrorPosition = pos})
        Token {position = pos} : _ ->
          case parseExprStatement tokenStream of
            Just statementResult -> Right statementResult
            Nothing -> Left (ExpectedExpression {parseErrorPosition = pos})
        [] -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
        tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
        [] -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
