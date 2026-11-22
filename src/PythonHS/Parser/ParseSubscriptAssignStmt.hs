module PythonHS.Parser.ParseSubscriptAssignStmt (parseSubscriptAssignStmt) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType (TokenType (AssignToken))
import PythonHS.Parser.ExprPos (exprPos)
import PythonHS.Parser.ParseAugAssignOperator (parseAugAssignOperator)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult (..))
import PythonHS.Parser.ParseSubscriptAssignStmtConfig (ParseSubscriptAssignStmtConfig (..))

-- | Parses assignment to an index or slice target, such as @xs[0] = 1@ or
-- @xs[0] += 1@. Returns Nothing when the statement is not a subscript
-- assignment.
parseSubscriptAssignStmt :: ParseSubscriptAssignStmtConfig -> Maybe (Either ParseError (ParseResult Stmt))
parseSubscriptAssignStmt config =
  case parseExpr tokenStream of
    Right targetResult ->
      case parseResultValue targetResult of
        target@IndexExpr {} -> parseAssignment target (parseResultRemaining targetResult)
        target@SliceExpr {} -> parseAssignment target (parseResultRemaining targetResult)
        _ -> Nothing
    Left _ -> Nothing
  where
    tokenStream = parseSubscriptAssignStmtTokenStream config
    parseExpr = parseSubscriptAssignStmtExpr config

    parseAssignment target remaining =
      case remaining of
        Token {tokenType = AssignToken} : rest -> Just (plainAssignment target rest)
        _ ->
          case parseAugAssignOperator remaining of
            Just (op, rest) -> Just (augmentedAssignment target op rest)
            Nothing -> Nothing

    plainAssignment target rest = do
      valueResult <- parseExpr rest
      Right
        ParseResult
          { parseResultValue =
              SubscriptAssignStmt
                { subscriptAssignStmtTarget = target,
                  subscriptAssignStmtValue = parseResultValue valueResult,
                  subscriptAssignStmtPos = exprPos target
                },
            parseResultRemaining = parseResultRemaining valueResult
          }

    augmentedAssignment target op rest = do
      valueResult <- parseExpr rest
      let valueExpr =
            BinaryExpr
              { binaryExprOp = op,
                binaryExprLeft = target,
                binaryExprRight = parseResultValue valueResult,
                binaryExprPos = exprPos target
              }
      Right
        ParseResult
          { parseResultValue =
              SubscriptAssignStmt
                { subscriptAssignStmtTarget = target,
                  subscriptAssignStmtValue = valueExpr,
                  subscriptAssignStmtPos = exprPos target
                },
            parseResultRemaining = parseResultRemaining valueResult
          }
