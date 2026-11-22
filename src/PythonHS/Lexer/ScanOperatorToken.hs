module PythonHS.Lexer.ScanOperatorToken (scanOperatorToken) where

import PythonHS.Lexer.LexerError (LexerError (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.ScanOperatorTokenConfig (ScanOperatorTokenConfig (..))
import PythonHS.Lexer.ScanTokenStepResult (ScanTokenStepResult (..))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType
  ( TokenType
      ( AmpersandAssignToken,
        AmpersandToken,
        AssignToken,
        CaretAssignToken,
        CaretToken,
        DoubleSlashAssignToken,
        DoubleSlashToken,
        DoubleStarAssignToken,
        DoubleStarToken,
        EqToken,
        GtToken,
        GteToken,
        LeftShiftAssignToken,
        LeftShiftToken,
        LtToken,
        LteToken,
        MinusAssignToken,
        MinusToken,
        NotEqToken,
        PercentAssignToken,
        PercentToken,
        PipeAssignToken,
        PipeToken,
        PlusAssignToken,
        PlusToken,
        RightShiftAssignToken,
        RightShiftToken,
        SlashAssignToken,
        SlashToken,
        StarAssignToken,
        StarToken,
        TildeToken
      ),
  )

-- | Scan punctuation that is an operator or a compound assignment.
scanOperatorToken :: ScanOperatorTokenConfig -> Maybe (Either LexerError ScanTokenStepResult)
scanOperatorToken config =
  case scanOperatorTokenCharacter config of
    '=' -> Just (operator '=' AssignToken '=' EqToken)
    '!' -> Just (operatorWithoutSingle NotEqToken)
    '<' -> Just (threeWay '<' LeftShiftToken LteToken LtToken LeftShiftAssignToken)
    '>' -> Just (threeWay '>' RightShiftToken GteToken GtToken RightShiftAssignToken)
    '&' -> Just (compound '&' AmpersandToken AmpersandAssignToken)
    '^' -> Just (compound '^' CaretToken CaretAssignToken)
    '|' -> Just (compound '|' PipeToken PipeAssignToken)
    '~' -> Just (Right (token TildeToken "~" rest 1))
    '+' -> Just (compound '+' PlusToken PlusAssignToken)
    '-' -> Just (compound '-' MinusToken MinusAssignToken)
    '*' -> Just (star)
    '/' -> Just (slash)
    '%' -> Just (compound '%' PercentToken PercentAssignToken)
    _ -> Nothing
  where
    rest = scanOperatorTokenRest config
    Position ln col = scanOperatorTokenPosition config

    token tokenType lexeme remaining consumed =
      ScanTokenStepResult
        (Token tokenType lexeme (Position ln col))
        remaining
        (Position ln (col + consumed))

    operatorWithoutSingle tokenType =
      case rest of
        '=' : rest' -> Right (token tokenType "!=" rest' 2)
        _ -> Left (UnexpectedCharacter {unexpectedChar = '!'})

    operator firstChar singleToken followingChar followingToken =
      case rest of
        next : rest'
          | next == firstChar -> Right (token followingToken [firstChar, followingChar] rest' 2)
          | next == followingChar -> Right (token followingToken [firstChar, followingChar] rest' 2)
        _ -> Right (token singleToken [firstChar] rest 1)

    threeWay firstChar middleToken lteToken singleToken assignToken =
      case rest of
        next : '=' : rest'
          | next == firstChar -> Right (token assignToken [firstChar, firstChar, '='] rest' 3)
        next : rest'
          | next == firstChar -> Right (token middleToken [firstChar, firstChar] rest' 2)
        '=' : rest' -> Right (token lteToken [firstChar, '='] rest' 2)
        _ -> Right (token singleToken [firstChar] rest 1)

    compound firstChar singleToken assignToken =
      case rest of
        '=' : rest' -> Right (token assignToken [firstChar, '='] rest' 2)
        _ -> Right (token singleToken [firstChar] rest 1)

    star =
      case rest of
        '*' : '=' : rest' -> Right (token DoubleStarAssignToken "**=" rest' 3)
        '*' : rest' -> Right (token DoubleStarToken "**" rest' 2)
        '=' : rest' -> Right (token StarAssignToken "*=" rest' 2)
        _ -> Right (token StarToken "*" rest 1)

    slash =
      case rest of
        '/' : '=' : rest' -> Right (token DoubleSlashAssignToken "//=" rest' 3)
        '/' : rest' -> Right (token DoubleSlashToken "//" rest' 2)
        '=' : rest' -> Right (token SlashAssignToken "/=" rest' 2)
        _ -> Right (token SlashToken "/" rest 1)
