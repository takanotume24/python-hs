module PythonHS.Parser.ParseDecoratedStmt (parseDecoratedStmt) where

import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (AtToken, NewlineToken))
import PythonHS.Parser.ParseDecoratedStmtConfig (ParseDecoratedStmtConfig (..))
import PythonHS.Parser.ParseError (ParseError (..))

parseDecoratedStmt ::
  ParseDecoratedStmtConfig ->
  [Token] ->
  Either ParseError (Stmt, [Token])
parseDecoratedStmt config tokenStream = do
  let parseExprFn = parseDecoratedStmtExpr config
      parseStatementFn = parseDecoratedStmtStatement config
      atPos = parseDecoratedStmtPos config
  (decorators, afterDecorators) <- parseDecoratorLines parseExprFn [] tokenStream
  (targetStmt, afterTarget) <- parseStatementFn afterDecorators
  case targetStmt of
    FunctionDefStmt {} -> Right (DecoratedStmt {decoratedStmtDecorators = decorators, decoratedStmtTarget = targetStmt, decoratedStmtPos = atPos}, afterTarget)
    FunctionDefDefaultsStmt {} -> Right (DecoratedStmt {decoratedStmtDecorators = decorators, decoratedStmtTarget = targetStmt, decoratedStmtPos = atPos}, afterTarget)
    ClassDefStmt {} -> Right (DecoratedStmt {decoratedStmtDecorators = decorators, decoratedStmtTarget = targetStmt, decoratedStmtPos = atPos}, afterTarget)
    _ ->
      case afterDecorators of
        tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
        _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
  where
    parseDecoratorLines parseExprFn acc (Token AtToken _ _ : rest) = do
      (decoratorExpr, afterExpr) <- parseExprFn rest
      case afterExpr of
        Token NewlineToken _ _ : afterNewline ->
          parseDecoratorLines parseExprFn (acc ++ [decoratorExpr]) afterNewline
        tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
        _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
    parseDecoratorLines _ acc remaining = Right (acc, remaining)
