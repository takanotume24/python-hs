module PythonHS.Parser.ParseFStringExpr (parseFStringExpr) where

import Data.Char (isSpace)
import PythonHS.AST.BinaryOperator (BinaryOperator (AddOperator))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.Lexer.ScanTokens (scanTokens)
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType (TokenType (DedentToken, EOFToken, IndentToken, NewlineToken))
import PythonHS.Parser.FStringInterpolation (FStringInterpolation (..))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseFStringExprConfig (ParseFStringExprConfig (..))
import PythonHS.Parser.ParseResult (ParseResult (..))

-- | Parses the contents of an f-string into a concatenation of literals and
-- @str(...)@ / @repr(...)@ / @__python_hs_format__(...)@ calls.
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
      interpolation <- splitInterpolation rest
      expr <- parseInterpolation (fStringInterpolationExpr interpolation)
      prependExpr (makePart expr interpolation) (parseParts (fStringInterpolationRemaining interpolation))
    parseParts ('}' : _) = Left (ExpectedExpression {parseErrorPosition = pos})
    parseParts (character : rest) = prependText [character] (parseParts rest)

    makePart expr interpolation =
      case fStringInterpolationSpec interpolation of
        Just formatSpec ->
          CallExpr
            { callExprName = "__python_hs_format__",
              callExprArgs = [convertedExpr expr interpolation, StringExpr {stringExprValue = formatSpec, stringExprPos = pos}],
              callExprPos = pos
            }
        Nothing ->
          case fStringInterpolationConversion interpolation of
            Nothing ->
              CallExpr
                { callExprName = "str",
                  callExprArgs = [expr],
                  callExprPos = pos
                }
            Just _ -> convertedExpr expr interpolation

    convertedExpr expr interpolation =
      case fStringInterpolationConversion interpolation of
        Just 'r' -> CallExpr {callExprName = "repr", callExprArgs = [expr], callExprPos = pos}
        Just 's' -> CallExpr {callExprName = "str", callExprArgs = [expr], callExprPos = pos}
        _ -> expr

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

    splitInterpolation = go [] (0 :: Int) Nothing
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
              | character == '}' ->
                  Right
                    FStringInterpolation
                      { fStringInterpolationExpr = acc,
                        fStringInterpolationConversion = Nothing,
                        fStringInterpolationSpec = Nothing,
                        fStringInterpolationRemaining = rest
                      }
              | character == '!' && depth == 0 -> readConversion acc rest
              | character == ':' && depth == 0 -> readFormat acc Nothing rest
              | otherwise -> go (acc ++ [character]) depth Nothing rest

        readConversion acc rest =
          case rest of
            conversion : afterConversion
              | conversion == 'r' || conversion == 's' -> readConversionTail acc conversion afterConversion
            _ -> Left (ExpectedExpression {parseErrorPosition = pos})

        readConversionTail acc conversion rest =
          case rest of
            ':' : specRest -> readFormat acc (Just conversion) specRest
            '}' : after ->
              Right
                FStringInterpolation
                  { fStringInterpolationExpr = acc,
                    fStringInterpolationConversion = Just conversion,
                    fStringInterpolationSpec = Nothing,
                    fStringInterpolationRemaining = after
                  }
            _ -> Left (ExpectedExpression {parseErrorPosition = pos})

        readFormat acc conversion rest =
          case break (== '}') rest of
            (formatSpec, '}' : after) ->
              Right
                FStringInterpolation
                  { fStringInterpolationExpr = acc,
                    fStringInterpolationConversion = conversion,
                    fStringInterpolationSpec = Just formatSpec,
                    fStringInterpolationRemaining = after
                  }
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
