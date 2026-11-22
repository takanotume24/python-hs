module PythonHS.Parser.DesugarStarredDict (desugarStarredDict) where

import PythonHS.AST.DictEntry (DictEntry (..))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.Lexer.Position (Position)

-- | Rewrite a dict display that contains @**value@ spreads into calls to the
-- existing @update@ builtin, starting from an empty dict.
desugarStarredDict :: Position -> [DictEntry Expr] -> Expr
desugarStarredDict dictPos entries =
  if any isSpreadEntry entries
    then foldl step (DictExpr {dictExprEntries = [], dictExprPos = dictPos}) entries
    else DictExpr {dictExprEntries = entries, dictExprPos = dictPos}
  where
    isSpreadEntry DictEntry {dictEntryKey = KwStarArgExpr {}} = True
    isSpreadEntry _ = False

    step accumulated entry =
      case entry of
        DictEntry {dictEntryKey = KwStarArgExpr {kwStarArgExprValue = value, kwStarArgExprPos = pos}} ->
          CallExpr {callExprName = "update", callExprArgs = [accumulated, value], callExprPos = pos}
        DictEntry {dictEntryKey = key, dictEntryValue = value} ->
          CallExpr
            { callExprName = "update",
              callExprArgs = [accumulated, DictExpr {dictExprEntries = [DictEntry {dictEntryKey = key, dictEntryValue = value}], dictExprPos = dictPos}],
              callExprPos = dictPos
            }
