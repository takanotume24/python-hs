module PythonHS.Parser.DesugarStarredList (desugarStarredList) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AddOperator))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.Lexer.Position (Position)
import PythonHS.Parser.ExprPos (exprPos)

-- | Rewrite a list display that contains @*value@ items into calls to the
-- existing @list@ and @append@ builtins.
desugarStarredList :: Position -> [Expr] -> Expr
desugarStarredList listPos items =
  if any isStarred items
    then foldl step (ListExpr {listExprItems = [], listExprPos = listPos}) items
    else ListExpr {listExprItems = items, listExprPos = listPos}
  where
    isStarred StarArgExpr {} = True
    isStarred _ = False

    step accumulated item =
      case item of
        StarArgExpr {starArgExprValue = value, starArgExprPos = starPos} ->
          BinaryExpr
            { binaryExprOp = AddOperator,
              binaryExprLeft = accumulated,
              binaryExprRight = CallExpr {callExprName = "list", callExprArgs = [value], callExprPos = starPos},
              binaryExprPos = starPos
            }
        _ ->
          CallExpr
            { callExprName = "append",
              callExprArgs = [accumulated, item],
              callExprPos = exprPos item
            }
