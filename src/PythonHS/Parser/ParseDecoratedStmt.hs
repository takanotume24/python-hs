module PythonHS.Parser.ParseDecoratedStmt (parseDecoratedStmt) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt (ClassDefStmt, DecoratedStmt, FunctionDefDefaultsStmt, FunctionDefStmt), Stmt)
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (AtToken, NewlineToken))
import PythonHS.Parser.ParseDecoratedStmtConfig (ParseDecoratedStmtConfig (..))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))

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
    FunctionDefStmt {} -> Right (DecoratedStmt decorators targetStmt atPos, afterTarget)
    FunctionDefDefaultsStmt {} -> Right (DecoratedStmt decorators targetStmt atPos, afterTarget)
    ClassDefStmt {} -> Right (DecoratedStmt decorators targetStmt atPos, afterTarget)
    _ ->
      case afterDecorators of
        tok : _ -> Left (ExpectedExpression (position tok))
        _ -> Left (ExpectedExpression (Position 0 0))
  where
    parseDecoratorLines parseExprFn acc (Token AtToken _ _ : rest) = do
      (decoratorExpr, afterExpr) <- parseExprFn rest
      case afterExpr of
        Token NewlineToken _ _ : afterNewline ->
          parseDecoratorLines parseExprFn (acc ++ [decoratorExpr]) afterNewline
        tok : _ -> Left (ExpectedExpression (position tok))
        _ -> Left (ExpectedExpression (Position 0 0))
    parseDecoratorLines _ acc remaining = Right (acc, remaining)
