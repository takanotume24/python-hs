module PythonHS.Parser.ParseAugAssignStmt (parseAugAssignStmt) where

import PythonHS.AST.BinaryOperator (BinaryOperator (..))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType
  ( TokenType
      ( DotToken,
        DoubleSlashAssignToken,
        DoubleStarAssignToken,
        IdentifierToken,
        MinusAssignToken,
        PercentAssignToken,
        PlusAssignToken,
        SlashAssignToken,
        StarAssignToken
      ),
  )
import PythonHS.Parser.ParseAugAssignStmtConfig (ParseAugAssignStmtConfig (..))
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult (..))

-- | Parses augmented assignment on a name or attribute target.
-- @**=@ is desugared to @x = x ** value@ so the existing binary operator
-- evaluation can be reused. Returns Nothing when the stream is not an
-- augmented assignment.
parseAugAssignStmt :: ParseAugAssignStmtConfig -> Maybe (Either ParseError (ParseResult Stmt))
parseAugAssignStmt config =
  case tokenStream of
    Token {tokenType = IdentifierToken, lexeme = obj, position = pos} : Token {tokenType = DotToken} : Token {tokenType = IdentifierToken, lexeme = attr} : opTokens ->
      parseWith (obj ++ "." ++ attr) pos opTokens
    Token {tokenType = IdentifierToken, lexeme = name, position = pos} : opTokens ->
      parseWith name pos opTokens
    _ -> Nothing
  where
    tokenStream = parseAugAssignStmtTokenStream config
    parseExpr = parseAugAssignStmtExpr config

    parseWith target pos opTokens =
      case splitOperator target pos opTokens of
        Nothing -> Nothing
        Just (makeStmt, rest) ->
          Just $ do
            valueResult <- parseExpr rest
            Right
              ParseResult
                { parseResultValue = makeStmt (parseResultValue valueResult),
                  parseResultRemaining = parseResultRemaining valueResult
                }

    splitOperator target pos opTokens =
      case opTokens of
        Token {tokenType = DoubleStarAssignToken} : rest ->
          Just (powerAssign target pos, rest)
        Token {tokenType = opType} : rest ->
          case opType of
            PlusAssignToken -> Just (addAssign target pos, rest)
            MinusAssignToken -> Just (subAssign target pos, rest)
            StarAssignToken -> Just (mulAssign target pos, rest)
            SlashAssignToken -> Just (divAssign target pos, rest)
            PercentAssignToken -> Just (modAssign target pos, rest)
            DoubleSlashAssignToken -> Just (floorDivAssign target pos, rest)
            _ -> Nothing
        _ -> Nothing

    powerAssign target pos valueExpr =
      AssignStmt
        { assignStmtName = target,
          assignStmtValue =
            BinaryExpr
              { binaryExprOp = PowerOperator,
                binaryExprLeft = IdentifierExpr {identifierExprName = target, identifierExprPos = pos},
                binaryExprRight = valueExpr,
                binaryExprPos = pos
              },
          assignStmtPos = pos
        }

    addAssign target pos valueExpr =
      AddAssignStmt {addAssignStmtName = target, addAssignStmtValue = valueExpr, addAssignStmtPos = pos}

    subAssign target pos valueExpr =
      SubAssignStmt {subAssignStmtName = target, subAssignStmtValue = valueExpr, subAssignStmtPos = pos}

    mulAssign target pos valueExpr =
      MulAssignStmt {mulAssignStmtName = target, mulAssignStmtValue = valueExpr, mulAssignStmtPos = pos}

    divAssign target pos valueExpr =
      DivAssignStmt {divAssignStmtName = target, divAssignStmtValue = valueExpr, divAssignStmtPos = pos}

    modAssign target pos valueExpr =
      ModAssignStmt {modAssignStmtName = target, modAssignStmtValue = valueExpr, modAssignStmtPos = pos}

    floorDivAssign target pos valueExpr =
      FloorDivAssignStmt {floorDivAssignStmtName = target, floorDivAssignStmtValue = valueExpr, floorDivAssignStmtPos = pos}
