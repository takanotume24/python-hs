module PythonHS.Parser.ParseUnpackAssign (parseUnpackAssign) where

import PythonHS.AST.Stmt (Stmt(AssignUnpackStmt))
import PythonHS.Lexer.Position (Position(Position))
import PythonHS.Lexer.Token (Token(Token))
import PythonHS.Lexer.TokenType
  ( TokenType
      ( AssignToken
      )
  )
import PythonHS.Parser.ParseError (ParseError(ExpectedExpression))
import PythonHS.Parser.ParseExpr (parseExpr)
import PythonHS.Parser.ParseUnpackNames (parseUnpackNames)

parseUnpackAssign :: String -> Position -> [Token] -> Either ParseError (Stmt, [Token])
parseUnpackAssign firstName pos rest = do
  (names, afterNames) <- parseUnpackNames [firstName] rest
  case afterNames of
    Token AssignToken _ _ : afterAssign -> do
      (valueExpr, remaining) <- parseExpr afterAssign
      Right (AssignUnpackStmt names valueExpr pos, remaining)
    Token _ _ pos' : _ -> Left (ExpectedExpression pos')
    _ -> Left (ExpectedExpression (Position 0 0))
