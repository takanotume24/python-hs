module PythonHS.Parser.ParseCallArgument (parseCallArgument) where

import PythonHS.AST.Expr (Expr (KeywordArgExpr, KwStarArgExpr, StarArgExpr), Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (AssignToken, IdentifierToken, StarToken))
import PythonHS.Parser.ExprPos (exprPos)
import PythonHS.Parser.ParseCallArgumentConfig (ParseCallArgumentConfig (..))
import PythonHS.Parser.ParseError (ParseError)

parseCallArgument ::
  ParseCallArgumentConfig ->
  [Token] ->
  Either ParseError (Expr, Bool, Position, [Token])
parseCallArgument config tokenStream =
  let parseExpr = parseCallArgumentExpr config
   in case tokenStream of
        Token IdentifierToken name namePos : Token AssignToken _ assignPos : rest -> do
          (valueExpr, afterValue) <- parseExpr rest
          Right (KeywordArgExpr name valueExpr namePos, True, assignPos, afterValue)
        Token StarToken _ starPos : Token StarToken _ _ : rest -> do
          (valueExpr, afterValue) <- parseExpr rest
          Right (KwStarArgExpr valueExpr starPos, True, starPos, afterValue)
        Token StarToken _ starPos : rest -> do
          (valueExpr, afterValue) <- parseExpr rest
          Right (StarArgExpr valueExpr starPos, False, starPos, afterValue)
        _ -> do
          (argExpr, afterArg) <- parseExpr tokenStream
          Right (argExpr, False, exprPos argExpr, afterArg)
