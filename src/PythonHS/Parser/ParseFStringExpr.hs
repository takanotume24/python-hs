module PythonHS.Parser.ParseFStringExpr (parseFStringExpr) where

import Data.Char (isSpace)
import PythonHS.AST.BinaryOperator (BinaryOperator (AddOperator))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.Lexer.ScanTokens (scanTokens)
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType (TokenType (DedentToken, EOFToken, IndentToken, NewlineToken))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseFStringExprConfig (ParseFStringExprConfig (..))
import PythonHS.Parser.ParseResult (ParseResult (..))

-- | Parses the contents of an f-string into a concatenation of literals and
-- @str(...)@ / @__python_hs_format__(...)@ calls.
parseFStringExpr :: ParseFStringExprConfig -> Either ParseError Expr
parseFStringExpr config = do
  parts <- parseParts content
  pure (concatParts parts)
  where
    content = parseFStringExprContent config
    pos = parseFStringExprPos config
    parseExprText = parseFStringExprParseExpr config

    parseParts [] = Right []
    parseParts ('{' : '{' : rest) = prependText "{" (parseParts rest)
    parseParts ('}' : '}' : rest) = prependText "}" (parseParts rest)
    parseParts ('{' : rest) = do
      (exprText, maybeSpec, after) <- splitInterpolation rest
      expr <- parseInterpolation exprText
      let part =
            case maybeSpec of
              Nothing ->
                CallExpr
                  { callExprName = "str",
                    callExprArgs = [expr],
                    callExprPos = pos
                  }
              Just spec ->
                CallExpr
                  { callExprName = "__python_hs_format__",
                    callExprArgs = [expr, StringExpr {stringExprValue = spec, stringExprPos = pos}],
                    callExprPos = pos
                  }
      prependExpr part (parseParts after)
    parseParts ('}' : _) = Left (ExpectedExpression {parseErrorPosition = pos})
    parseParts (character : rest) = prependText [character] (parseParts rest)

    prependText text restParts =
      fmap (StringExpr {stringExprValue = text, stringExprPos = pos} :) restParts

    prependExpr expr restParts = fmap (expr :) restParts

    concatParts [] = StringExpr {stringExprValue = "", stringExprPos = pos}
    concatParts (firstPart : restParts) = foldl addPart firstPart restParts

    addPart left right =
      BinaryExpr
        { binaryExprOp = AddOperator,
          binaryExprLeft = left,
          binaryExprRight = right,
          binaryExprPos = pos
        }

    splitInterpolation = go [] 0 Nothing
      where
        go _ _ _ [] = Left (ExpectedExpression {parseErrorPosition = pos})
        go acc depth quote (character : rest) =
          case quote of
            Just quoteChar
              | character == quoteChar -> go (acc ++ [character]) depth Nothing rest
              | character == '\\' ->
                  case rest of
                    escapedChar : rest' -> go (acc ++ ['\\', escapedChar]) depth (Just quoteChar) rest'
                    [] -> Left (ExpectedExpression {parseErrorPosition = pos})
              | otherwise -> go (acc ++ [character]) depth (Just quoteChar) rest
            Nothing
              | character == '\'' || character == '"' -> go (acc ++ [character]) depth (Just character) rest
              | character == '{' -> go (acc ++ [character]) (depth + 1) Nothing rest
              | character == '}' && depth > 0 -> go (acc ++ [character]) (depth - 1) Nothing rest
              | character == '}' -> Right (acc, Nothing, rest)
              | character == ':' && depth == 0 -> readFormat acc rest
              | otherwise -> go (acc ++ [character]) depth Nothing rest

        readFormat acc rest =
          case break (== '}') rest of
            (formatSpec, '}' : after) -> Right (acc, Just formatSpec, after)
            _ -> Left (ExpectedExpression {parseErrorPosition = pos})

    parseInterpolation text =
      case scanTokens (trim text) of
        Left _ -> Left (ExpectedExpression {parseErrorPosition = pos})
        Right tokens ->
          let exprTokens = takeWhile (not . isLayoutToken) tokens
           in case parseExprText exprTokens of
                Left err -> Left err
                Right result ->
                  case parseResultRemaining result of
                    [] -> Right (parseResultValue result)
                    _ -> Left (ExpectedExpression {parseErrorPosition = pos})

    isLayoutToken token =
      tokenType token `elem` [NewlineToken, EOFToken, IndentToken, DedentToken]

    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
