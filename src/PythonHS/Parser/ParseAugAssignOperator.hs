module PythonHS.Parser.ParseAugAssignOperator (parseAugAssignOperator) where

import PythonHS.AST.BinaryOperator
  ( BinaryOperator
      ( AddOperator,
        BitwiseAndOperator,
        BitwiseOrOperator,
        BitwiseXorOperator,
        DivideOperator,
        FloorDivideOperator,
        LeftShiftOperator,
        ModuloOperator,
        MultiplyOperator,
        PowerOperator,
        RightShiftOperator,
        SubtractOperator
      ),
  )
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType
  ( TokenType
      ( AmpersandAssignToken,
        CaretAssignToken,
        DoubleSlashAssignToken,
        DoubleStarAssignToken,
        LeftShiftAssignToken,
        MinusAssignToken,
        PercentAssignToken,
        PipeAssignToken,
        PlusAssignToken,
        RightShiftAssignToken,
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
    Token {tokenType = AmpersandAssignToken} : rest -> Just (BitwiseAndOperator, rest)
    Token {tokenType = PipeAssignToken} : rest -> Just (BitwiseOrOperator, rest)
    Token {tokenType = CaretAssignToken} : rest -> Just (BitwiseXorOperator, rest)
    Token {tokenType = LeftShiftAssignToken} : rest -> Just (LeftShiftOperator, rest)
    Token {tokenType = RightShiftAssignToken} : rest -> Just (RightShiftOperator, rest)
    _ -> Nothing
