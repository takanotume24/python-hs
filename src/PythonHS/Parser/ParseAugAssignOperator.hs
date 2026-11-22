module PythonHS.Parser.ParseAugAssignOperator (parseAugAssignOperator) where

import PythonHS.AST.BinaryOperator
  ( BinaryOperator
      ( AddOperator,
        DivideOperator,
        FloorDivideOperator,
        ModuloOperator,
        MultiplyOperator,
        PowerOperator,
        SubtractOperator
      ),
  )
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType
  ( TokenType
      ( DoubleSlashAssignToken,
        DoubleStarAssignToken,
        MinusAssignToken,
        PercentAssignToken,
        PlusAssignToken,
        SlashAssignToken,
        StarAssignToken
      ),
  )

-- | Recognises an augmented assignment operator at the head of a token stream
-- and returns the matching binary operator with the remaining tokens.
parseAugAssignOperator :: [Token] -> Maybe (BinaryOperator, [Token])
parseAugAssignOperator tokens =
  case tokens of
    Token {tokenType = PlusAssignToken} : rest -> Just (AddOperator, rest)
    Token {tokenType = MinusAssignToken} : rest -> Just (SubtractOperator, rest)
    Token {tokenType = StarAssignToken} : rest -> Just (MultiplyOperator, rest)
    Token {tokenType = SlashAssignToken} : rest -> Just (DivideOperator, rest)
    Token {tokenType = DoubleSlashAssignToken} : rest -> Just (FloorDivideOperator, rest)
    Token {tokenType = PercentAssignToken} : rest -> Just (ModuloOperator, rest)
    Token {tokenType = DoubleStarAssignToken} : rest -> Just (PowerOperator, rest)
    _ -> Nothing
