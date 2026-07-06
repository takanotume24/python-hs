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
        ForToken,
        FromToken,
        GlobalToken,
        IdentifierToken,
        IfToken,
        ImportToken,
        MatchToken,
        MinusAssignToken,
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
import PythonHS.Parser.ParseAnnAssignStmt (parseAnnAssignStmt)
import PythonHS.Parser.ParseAnnAssignStmtConfig (ParseAnnAssignStmtConfig (..))
import PythonHS.Parser.ParseClassStmt (parseClassStmt)
import PythonHS.Parser.ParseClassStmtConfig (ParseClassStmtConfig (..))
import PythonHS.Parser.ParseCompoundStmt (parseCompoundStmt)
import PythonHS.Parser.ParseCompoundStmtConfig (ParseCompoundStmtConfig (..))
import PythonHS.Parser.ParseDecoratedStmt (parseDecoratedStmt)
import PythonHS.Parser.ParseDecoratedStmtConfig (ParseDecoratedStmtConfig (..))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseExpr (parseExpr)
import PythonHS.Parser.ParseImportStmt (parseImportStmt)
import PythonHS.Parser.ParseMatchStmt (parseMatchStmt)
import PythonHS.Parser.ParseMatchStmtConfig (ParseMatchStmtConfig (..))
import PythonHS.Parser.ParseResult (ParseResult (..))
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
   in case tokenStream of
        Token {tokenType = AtToken, position = pos} : _ ->
          parseDecoratedStmt (ParseDecoratedStmtConfig {parseDecoratedStmtExpr = parseExpr, parseDecoratedStmtStatement = parseStatement, parseDecoratedStmtPos = pos, parseDecoratedStmtTokenStream = tokenStream})
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
        Token {tokenType = RaiseToken, position = pos} : rest -> do
          valueResult <- parseExpr rest
          let valueExpr = parseResultValue valueResult
              remaining = parseResultRemaining valueResult
          Right (ParseResult {parseResultValue = RaiseStmt {raiseStmtExpr = valueExpr, raiseStmtPos = pos}, parseResultRemaining = remaining})
        Token {tokenType = BreakToken, position = pos} : rest -> Right (ParseResult {parseResultValue = BreakStmt {breakStmtPos = pos}, parseResultRemaining = rest})
        Token {tokenType = ContinueToken, position = pos} : rest -> Right (ParseResult {parseResultValue = ContinueStmt {continueStmtPos = pos}, parseResultRemaining = rest})
        Token {tokenType = PassToken, position = pos} : rest -> Right (ParseResult {parseResultValue = PassStmt {passStmtPos = pos}, parseResultRemaining = rest})
        Token {tokenType = GlobalToken, position = pos} : Token {tokenType = IdentifierToken, lexeme = name} : rest ->
          Right (ParseResult {parseResultValue = GlobalStmt {globalStmtName = name, globalStmtPos = pos}, parseResultRemaining = rest})
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
        Token {tokenType = IdentifierToken, lexeme = name, position = pos} : Token {tokenType = PlusAssignToken} : rest -> do
          valueResult <- parseExpr rest
          let valueExpr = parseResultValue valueResult
              remaining = parseResultRemaining valueResult
          Right (ParseResult {parseResultValue = AddAssignStmt {addAssignStmtName = name, addAssignStmtValue = valueExpr, addAssignStmtPos = pos}, parseResultRemaining = remaining})
        Token {tokenType = IdentifierToken, lexeme = name, position = pos} : Token {tokenType = MinusAssignToken} : rest -> do
          valueResult <- parseExpr rest
          let valueExpr = parseResultValue valueResult
              remaining = parseResultRemaining valueResult
          Right (ParseResult {parseResultValue = SubAssignStmt {subAssignStmtName = name, subAssignStmtValue = valueExpr, subAssignStmtPos = pos}, parseResultRemaining = remaining})
        Token {tokenType = IdentifierToken, lexeme = name, position = pos} : Token {tokenType = StarAssignToken} : rest -> do
          valueResult <- parseExpr rest
          let valueExpr = parseResultValue valueResult
              remaining = parseResultRemaining valueResult
          Right (ParseResult {parseResultValue = MulAssignStmt {mulAssignStmtName = name, mulAssignStmtValue = valueExpr, mulAssignStmtPos = pos}, parseResultRemaining = remaining})
        Token {tokenType = IdentifierToken, lexeme = name, position = pos} : Token {tokenType = SlashAssignToken} : rest -> do
          valueResult <- parseExpr rest
          let valueExpr = parseResultValue valueResult
              remaining = parseResultRemaining valueResult
          Right (ParseResult {parseResultValue = DivAssignStmt {divAssignStmtName = name, divAssignStmtValue = valueExpr, divAssignStmtPos = pos}, parseResultRemaining = remaining})
        Token {tokenType = IdentifierToken, lexeme = name, position = pos} : Token {tokenType = PercentAssignToken} : rest -> do
          valueResult <- parseExpr rest
          let valueExpr = parseResultValue valueResult
              remaining = parseResultRemaining valueResult
          Right (ParseResult {parseResultValue = ModAssignStmt {modAssignStmtName = name, modAssignStmtValue = valueExpr, modAssignStmtPos = pos}, parseResultRemaining = remaining})
        Token {tokenType = IdentifierToken, lexeme = name, position = pos} : Token {tokenType = DoubleSlashAssignToken} : rest -> do
          valueResult <- parseExpr rest
          let valueExpr = parseResultValue valueResult
              remaining = parseResultRemaining valueResult
          Right (ParseResult {parseResultValue = FloorDivAssignStmt {floorDivAssignStmtName = name, floorDivAssignStmtValue = valueExpr, floorDivAssignStmtPos = pos}, parseResultRemaining = remaining})
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
        Token {tokenType = IdentifierToken, position = pos} : _ -> Left (ExpectedAssignAfterIdentifier {parseErrorPosition = pos})
        tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
        [] -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
