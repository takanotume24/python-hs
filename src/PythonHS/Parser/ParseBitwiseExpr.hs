module PythonHS.Parser.ParseBitwiseExpr (parseBitwiseExpr) where

import PythonHS.AST.BinaryOperator (BinaryOperator (BitwiseAndOperator, BitwiseOrOperator, BitwiseXorOperator, LeftShiftOperator, RightShiftOperator))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (AmpersandToken, CaretToken, LeftShiftToken, PipeToken, RightShiftToken))
import PythonHS.Parser.ParseBitwiseExprConfig (ParseBitwiseExprConfig (..))
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult (..))

parseBitwiseExpr :: ParseBitwiseExprConfig -> Either ParseError (ParseResult Expr)
parseBitwiseExpr config = parseBitwiseOr nextParser tokens
  where
    nextParser = parseBitwiseExprConfigNextParser config
    tokens = parseBitwiseExprConfigTokens config

    parseBitwiseOr parseNext ts = parseLeftAssoc (parseBitwiseXor parseNext) operatorForBitwiseOr ts

    parseBitwiseXor parseNext ts = parseLeftAssoc (parseBitwiseAnd parseNext) operatorForBitwiseXor ts

    parseBitwiseAnd parseNext ts = parseLeftAssoc (parseShift parseNext) operatorForBitwiseAnd ts

    parseShift parseNext ts = parseLeftAssoc parseNext operatorForShift ts

    operatorForBitwiseOr PipeToken = Just BitwiseOrOperator
    operatorForBitwiseOr _ = Nothing

    operatorForBitwiseXor CaretToken = Just BitwiseXorOperator
    operatorForBitwiseXor _ = Nothing

    operatorForBitwiseAnd AmpersandToken = Just BitwiseAndOperator
    operatorForBitwiseAnd _ = Nothing

    operatorForShift LeftShiftToken = Just LeftShiftOperator
    operatorForShift RightShiftToken = Just RightShiftOperator
    operatorForShift _ = Nothing

    parseLeftAssoc nextParser' lookupOp ts = do
      initialResult <- nextParser' ts
      parseLeftAssocTail nextParser' lookupOp (parseResultValue initialResult) (parseResultRemaining initialResult)

    parseLeftAssocTail nextParser' lookupOp left rest@(Token tok _ pos : rest') =
      case lookupOp tok of
        Just op -> do
          rightResult <- nextParser' rest'
          parseLeftAssocTail nextParser' lookupOp (BinaryExpr {binaryExprOp = op, binaryExprLeft = left, binaryExprRight = parseResultValue rightResult, binaryExprPos = pos}) (parseResultRemaining rightResult)
        Nothing -> Right (ParseResult {parseResultValue = left, parseResultRemaining = rest})
    parseLeftAssocTail _ _ left rest = Right (ParseResult {parseResultValue = left, parseResultRemaining = rest})
