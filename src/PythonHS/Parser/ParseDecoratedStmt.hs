module PythonHS.Parser.ParseDecoratedStmt (parseDecoratedStmt) where

import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (AtToken, NewlineToken))
import PythonHS.Parser.ParseDecoratedStmtConfig (ParseDecoratedStmtConfig (..))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseResult (ParseResult (..))

parseDecoratedStmt ::
  ParseDecoratedStmtConfig ->
  Either ParseError (ParseResult Stmt)
parseDecoratedStmt config = do
  let parseExprFn = parseDecoratedStmtExpr config
      parseStatementFn = parseDecoratedStmtStatement config
      atPos = parseDecoratedStmtPos config
      tokenStream = parseDecoratedStmtTokenStream config
  decoratorsResult <- parseDecoratorLines parseExprFn [] tokenStream
  let decorators = parseResultValue decoratorsResult
      afterDecorators = parseResultRemaining decoratorsResult
  targetResult <- parseStatementFn afterDecorators
  let targetStmt = parseResultValue targetResult
      afterTarget = parseResultRemaining targetResult
  case targetStmt of
    FunctionDefStmt {} -> Right (ParseResult {parseResultValue = DecoratedStmt {decoratedStmtDecorators = decorators, decoratedStmtTarget = targetStmt, decoratedStmtPos = atPos}, parseResultRemaining = afterTarget})
    FunctionDefDefaultsStmt {} -> Right (ParseResult {parseResultValue = DecoratedStmt {decoratedStmtDecorators = decorators, decoratedStmtTarget = targetStmt, decoratedStmtPos = atPos}, parseResultRemaining = afterTarget})
    ClassDefStmt {} -> Right (ParseResult {parseResultValue = DecoratedStmt {decoratedStmtDecorators = decorators, decoratedStmtTarget = targetStmt, decoratedStmtPos = atPos}, parseResultRemaining = afterTarget})
    _ ->
      case afterDecorators of
        tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
        _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
  where
    parseDecoratorLines parseExprFn acc (Token AtToken _ _ : rest) = do
      exprResult <- parseExprFn rest
      let decoratorExpr = parseResultValue exprResult
          afterExpr = parseResultRemaining exprResult
      case afterExpr of
        Token NewlineToken _ _ : afterNewline ->
          parseDecoratorLines parseExprFn (acc ++ [decoratorExpr]) afterNewline
        tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
        _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
    parseDecoratorLines _ acc remaining = Right (ParseResult {parseResultValue = acc, parseResultRemaining = remaining})
