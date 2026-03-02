module PythonHS.Parser.ParseDecoratedStmt (parseDecoratedStmt) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt (ClassDefStmt, DecoratedStmt, FunctionDefDefaultsStmt, FunctionDefStmt), Stmt)
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (AtToken, NewlineToken))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))

parseDecoratedStmt ::
  ([Token] -> Either ParseError (Expr, [Token])) ->
  ([Token] -> Either ParseError (Stmt, [Token])) ->
  Position ->
  [Token] ->
  Either ParseError (Stmt, [Token])
parseDecoratedStmt parseExpr parseStatement atPos tokenStream = do
  (decorators, afterDecorators) <- parseDecoratorLines [] tokenStream
  (targetStmt, afterTarget) <- parseStatement afterDecorators
  case targetStmt of
    FunctionDefStmt {} -> Right (DecoratedStmt decorators targetStmt atPos, afterTarget)
    FunctionDefDefaultsStmt {} -> Right (DecoratedStmt decorators targetStmt atPos, afterTarget)
    ClassDefStmt {} -> Right (DecoratedStmt decorators targetStmt atPos, afterTarget)
    _ ->
      case afterDecorators of
        tok : _ -> Left (ExpectedExpression (position tok))
        _ -> Left (ExpectedExpression (Position 0 0))
  where
    parseDecoratorLines acc (Token AtToken _ _ : rest) = do
      (decoratorExpr, afterExpr) <- parseExpr rest
      case afterExpr of
        Token NewlineToken _ _ : afterNewline ->
          parseDecoratorLines (acc ++ [decoratorExpr]) afterNewline
        tok : _ -> Left (ExpectedExpression (position tok))
        _ -> Left (ExpectedExpression (Position 0 0))
    parseDecoratorLines acc remaining = Right (acc, remaining)
