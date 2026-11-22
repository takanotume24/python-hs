module PythonHS.Parser.DesugarStarredTuple (desugarStarredTuple) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.Lexer.Position (Position)
import PythonHS.Parser.DesugarStarredList (desugarStarredList)

-- | Rewrite a tuple display that contains @*value@ items by building the
-- equivalent list display and converting it with the @tuple@ builtin.
desugarStarredTuple :: Position -> [Expr] -> Expr
desugarStarredTuple tuplePos items =
  if any isStarred items
    then CallExpr {callExprName = "tuple", callExprArgs = [desugarStarredList tuplePos items], callExprPos = tuplePos}
    else TupleExpr {tupleExprItems = items, tupleExprPos = tuplePos}
  where
    isStarred StarArgExpr {} = True
    isStarred _ = False
