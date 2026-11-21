module PythonHS.Parser.ParseCompoundStmt (parseCompoundStmt) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType
  ( TokenType
      ( ColonToken,
        CommaToken,
        DefToken,
        ElseToken,
        ExceptToken,
        FinallyToken,
        ForToken,
        GtToken,
        IdentifierToken,
        IfToken,
        InToken,
        LParenToken,
        MinusToken,
        TryToken,
        WhileToken
      ),
  )
import PythonHS.Parser.DropLeadingNewlines (dropLeadingNewlines)
import PythonHS.Parser.ParseCompoundStmtConfig (ParseCompoundStmtConfig (..))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseExceptSuites (parseExceptSuites)
import PythonHS.Parser.ParseExceptSuitesConfig (ParseExceptSuitesConfig (..))
import PythonHS.Parser.ParseIfTail (parseIfTail)
import PythonHS.Parser.ParseIfTailConfig (ParseIfTailConfig (..))
import PythonHS.Parser.ParseParameters (parseParameters)
import PythonHS.Parser.ParseParametersResult (ParseParametersResult (..))
import PythonHS.Parser.ParseResult (ParseResult (..))
import PythonHS.Parser.ParseSuite (parseSuite)
import PythonHS.Parser.ParseSuiteConfig (ParseSuiteConfig (..))

parseCompoundStmt :: ParseCompoundStmtConfig -> Either ParseError (ParseResult Stmt)
parseCompoundStmt config =
  let tokenStream = parseCompoundStmtTokenStream config
      parseExpr = parseCompoundStmtExpr config
   in case tokenStream of
        Token {tokenType = IfToken, position = pos} : rest -> do
          condResult <- parseExpr rest
          let cond = parseResultValue condResult
              afterCond = parseResultRemaining condResult
          case afterCond of
            Token {tokenType = ColonToken} : afterColon -> do
              thenResult <- parseSuiteWithStatements afterColon
              let thenSuite = parseResultValue thenResult
                  afterThen = parseResultRemaining thenResult
              ifTailResult <- parseIfTail (ParseIfTailConfig {parseIfTailSuite = parseSuiteWithStatements, parseIfTailTokenStream = afterThen})
              let elseBranch = parseResultValue ifTailResult
                  finalRest = parseResultRemaining ifTailResult
              Right (ParseResult {parseResultValue = IfStmt {ifStmtCond = cond, ifStmtThen = thenSuite, ifStmtElse = elseBranch, ifStmtPos = pos}, parseResultRemaining = finalRest})
            Token {position = pos'} : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
        Token {tokenType = TryToken, position = pos} : rest ->
          case rest of
            Token {tokenType = ColonToken} : afterColon -> do
              tryResult <- parseSuiteWithStatements afterColon
              let trySuite = parseResultValue tryResult
                  afterTrySuite = dropLeadingNewlines (parseResultRemaining tryResult)
              exceptResult <- parseOptionalExceptSuites afterTrySuite
              let exceptSuites = parseResultValue exceptResult
                  afterExceptSuites = parseResultRemaining exceptResult
              elseResult <- parseElseSuiteTail afterExceptSuites
              finallyResult <- parseFinallySuiteTail (parseResultRemaining elseResult)
              Right (ParseResult {parseResultValue = TryExceptStmt {tryExceptStmtTryBody = trySuite, tryExceptStmtExceptSuites = exceptSuites, tryExceptStmtElseBody = parseResultValue elseResult, tryExceptStmtFinallyBody = parseResultValue finallyResult, tryExceptStmtPos = pos}, parseResultRemaining = parseResultRemaining finallyResult})
            Token {position = pos'} : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
        Token {tokenType = WhileToken, position = pos} : rest -> do
          condResult <- parseExpr rest
          let cond = parseResultValue condResult
              afterCond = parseResultRemaining condResult
          case afterCond of
            Token {tokenType = ColonToken} : afterColon -> do
              suiteResult <- parseSuiteWithStatements afterColon
              let bodySuite = parseResultValue suiteResult
                  finalRest = parseResultRemaining suiteResult
              Right (ParseResult {parseResultValue = WhileStmt {whileStmtCond = cond, whileStmtBody = bodySuite, whileStmtPos = pos}, parseResultRemaining = finalRest})
            Token {position = pos'} : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
        Token {tokenType = ForToken, position = pos} : Token {tokenType = IdentifierToken, lexeme = firstName} : afterFirstTarget -> do
          let targetsResult = collectForTargets [firstName] afterFirstTarget
              targets = parseResultValue targetsResult
              afterTargets = parseResultRemaining targetsResult
          case afterTargets of
            Token {tokenType = InToken} : rest -> do
              iterResult <- parseExpr rest
              let iterExpr = parseResultValue iterResult
                  afterIter = parseResultRemaining iterResult
              case afterIter of
                Token {tokenType = ColonToken} : afterColon -> do
                  suiteResult <- parseSuiteWithStatements afterColon
                  let bodySuite = desugarForTargets pos targets (parseResultValue suiteResult)
                      finalRest = parseResultRemaining suiteResult
                  Right (ParseResult {parseResultValue = ForStmt {forStmtVar = forLoopVariable targets, forStmtIter = iterExpr, forStmtBody = bodySuite, forStmtPos = pos}, parseResultRemaining = finalRest})
                Token {position = pos'} : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
                _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
            Token {position = pos'} : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
        Token {tokenType = DefToken, position = posDef} : Token {tokenType = IdentifierToken, lexeme = name} : Token {tokenType = LParenToken} : rest -> do
          paramsResult <- parseParameters parseExpr rest
          let params = parseParametersResultParams (parseResultValue paramsResult)
              defaults = parseParametersResultDefaults (parseResultValue paramsResult)
              afterParams = parseResultRemaining paramsResult
          case afterParams of
            Token {tokenType = MinusToken} : Token {tokenType = GtToken} : afterArrow -> do
              annotationResult <- parseExpr afterArrow
              let afterAnnotation = parseResultRemaining annotationResult
              case afterAnnotation of
                Token {tokenType = ColonToken} : afterColon -> do
                  suiteResult <- parseSuiteWithStatements afterColon
                  let bodySuite = parseResultValue suiteResult
                      finalRest = parseResultRemaining suiteResult
                  if null defaults
                    then Right (ParseResult {parseResultValue = FunctionDefStmt {functionDefStmtName = name, functionDefStmtParams = params, functionDefStmtBody = bodySuite, functionDefStmtPos = posDef}, parseResultRemaining = finalRest})
                    else Right (ParseResult {parseResultValue = FunctionDefDefaultsStmt {functionDefDefaultsStmtName = name, functionDefDefaultsStmtParams = params, functionDefDefaultsStmtDefaults = defaults, functionDefDefaultsStmtBody = bodySuite, functionDefDefaultsStmtPos = posDef}, parseResultRemaining = finalRest})
                Token {position = pos'} : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
                _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
            Token {tokenType = ColonToken} : afterColon -> do
              suiteResult <- parseSuiteWithStatements afterColon
              let bodySuite = parseResultValue suiteResult
                  finalRest = parseResultRemaining suiteResult
              if null defaults
                then Right (ParseResult {parseResultValue = FunctionDefStmt {functionDefStmtName = name, functionDefStmtParams = params, functionDefStmtBody = bodySuite, functionDefStmtPos = posDef}, parseResultRemaining = finalRest})
                else Right (ParseResult {parseResultValue = FunctionDefDefaultsStmt {functionDefDefaultsStmtName = name, functionDefDefaultsStmtParams = params, functionDefDefaultsStmtDefaults = defaults, functionDefDefaultsStmtBody = bodySuite, functionDefDefaultsStmtPos = posDef}, parseResultRemaining = finalRest})
            Token {position = pos'} : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
        _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
  where
    parseSuiteWithStatements tokens = parseSuite (ParseSuiteConfig {parseSuiteStatement = parseCompoundStmtStatement config, parseSuiteTokenStream = tokens})

    forTargetTemporaryName = "__python_hs_for_target__"

    collectForTargets names tokens =
      case tokens of
        Token {tokenType = CommaToken} : Token {tokenType = IdentifierToken, lexeme = nextName} : rest ->
          collectForTargets (names ++ [nextName]) rest
        _ -> ParseResult {parseResultValue = names, parseResultRemaining = tokens}

    forLoopVariable targets =
      case targets of
        [singleName] -> singleName
        _ -> forTargetTemporaryName

    desugarForTargets pos targets body =
      case targets of
        [_] -> body
        _ ->
          AssignUnpackStmt
            { assignUnpackStmtNames = targets,
              assignUnpackStmtValue = IdentifierExpr {identifierExprName = forTargetTemporaryName, identifierExprPos = pos},
              assignUnpackStmtPos = pos
            }
            : body

    parseOptionalExceptSuites tokens =
      case tokens of
        Token {tokenType = ExceptToken} : _ ->
          parseExceptSuites (ParseExceptSuitesConfig {parseExceptSuitesSuite = parseSuiteWithStatements, parseExceptSuitesTokenStream = tokens})
        _ -> Right (ParseResult {parseResultValue = [], parseResultRemaining = tokens})

    parseElseSuiteTail tokens =
      case dropLeadingNewlines tokens of
        Token {tokenType = ElseToken} : Token {tokenType = ColonToken} : afterElseColon -> do
          elseResult <- parseSuiteWithStatements afterElseColon
          Right (ParseResult {parseResultValue = Just (parseResultValue elseResult), parseResultRemaining = parseResultRemaining elseResult})
        Token {tokenType = ElseToken} : Token {position = pos} : _ -> Left (ExpectedExpression {parseErrorPosition = pos})
        _ -> Right (ParseResult {parseResultValue = Nothing, parseResultRemaining = tokens})

    parseFinallySuiteTail tokens =
      case dropLeadingNewlines tokens of
        Token {tokenType = FinallyToken} : Token {tokenType = ColonToken} : afterFinallyColon -> do
          finallyResult <- parseSuiteWithStatements afterFinallyColon
          Right (ParseResult {parseResultValue = Just (parseResultValue finallyResult), parseResultRemaining = parseResultRemaining finallyResult})
        _ -> Right (ParseResult {parseResultValue = Nothing, parseResultRemaining = tokens})
