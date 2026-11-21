module PythonHS.VM.RewriteClassAttrRefs (rewriteClassAttrRefs) where

import PythonHS.AST.DictEntry (DictEntry (..))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.ListComprehensionClause (ListComprehensionClause (..))
import PythonHS.VM.RewriteClassAttrRefsConfig (RewriteClassAttrRefsConfig (..))

rewriteClassAttrRefs :: RewriteClassAttrRefsConfig -> Expr
rewriteClassAttrRefs config = case rewriteClassAttrRefsConfigExpr config of
  IdentifierExpr {identifierExprName = name, identifierExprPos = pos} ->
    if name `elem` attrNames
      then IdentifierExpr {identifierExprName = className ++ "." ++ name, identifierExprPos = pos}
      else expr
  BinaryExpr {binaryExprOp = op, binaryExprLeft = left, binaryExprRight = right, binaryExprPos = pos} ->
    BinaryExpr {binaryExprOp = op, binaryExprLeft = rewrite left, binaryExprRight = rewrite right, binaryExprPos = pos}
  UnaryMinusExpr {unaryMinusExprValue = value, unaryMinusExprPos = pos} ->
    UnaryMinusExpr {unaryMinusExprValue = rewrite value, unaryMinusExprPos = pos}
  NotExpr {notExprValue = value, notExprPos = pos} ->
    NotExpr {notExprValue = rewrite value, notExprPos = pos}
  BitwiseNotExpr {bitwiseNotExprValue = value, bitwiseNotExprPos = pos} ->
    BitwiseNotExpr {bitwiseNotExprValue = rewrite value, bitwiseNotExprPos = pos}
  ComparisonChainExpr {comparisonChainLeft = left, comparisonChainOps = ops, comparisonChainRights = rights, comparisonChainPos = pos} ->
    ComparisonChainExpr {comparisonChainLeft = rewrite left, comparisonChainOps = ops, comparisonChainRights = map rewrite rights, comparisonChainPos = pos}
  CallExpr {callExprName = name, callExprArgs = args, callExprPos = pos} ->
    CallExpr {callExprName = name, callExprArgs = map rewrite args, callExprPos = pos}
  CallValueExpr {callValueExprCallee = callee, callValueExprArgs = args, callValueExprPos = pos} ->
    CallValueExpr {callValueExprCallee = rewrite callee, callValueExprArgs = map rewrite args, callValueExprPos = pos}
  ListExpr {listExprItems = items, listExprPos = pos} ->
    ListExpr {listExprItems = map rewrite items, listExprPos = pos}
  TupleExpr {tupleExprItems = items, tupleExprPos = pos} ->
    TupleExpr {tupleExprItems = map rewrite items, tupleExprPos = pos}
  DictExpr {dictExprEntries = entries, dictExprPos = pos} ->
    DictExpr {dictExprEntries = map (\entry -> entry {dictEntryKey = rewrite (dictEntryKey entry), dictEntryValue = rewrite (dictEntryValue entry)}) entries, dictExprPos = pos}
  IndexExpr {indexExprBase = base, indexExprIndex = index, indexExprPos = pos} ->
    IndexExpr {indexExprBase = rewrite base, indexExprIndex = rewrite index, indexExprPos = pos}
  SliceExpr {sliceExprBase = base, sliceExprStart = start, sliceExprEnd = end, sliceExprPos = pos} ->
    SliceExpr {sliceExprBase = rewrite base, sliceExprStart = fmap rewrite start, sliceExprEnd = fmap rewrite end, sliceExprPos = pos}
  ListComprehensionExpr {listComprehensionExprValue = value, listComprehensionExprLoopName = loopName, listComprehensionExprIter = iter, listComprehensionExprPos = pos} ->
    ListComprehensionExpr {listComprehensionExprValue = rewrite value, listComprehensionExprLoopName = loopName, listComprehensionExprIter = rewrite iter, listComprehensionExprPos = pos}
  ListComprehensionClausesExpr {listComprehensionClausesExprValue = value, listComprehensionClausesExprClauses = clauses, listComprehensionClausesExprPos = pos} ->
    ListComprehensionClausesExpr {listComprehensionClausesExprValue = rewrite value, listComprehensionClausesExprClauses = map rewriteClause clauses, listComprehensionClausesExprPos = pos}
  WalrusExpr {walrusExprName = name, walrusExprValue = value, walrusExprPos = pos} ->
    WalrusExpr {walrusExprName = name, walrusExprValue = rewrite value, walrusExprPos = pos}
  _ -> expr
  where
    className = rewriteClassAttrRefsConfigClassName config
    attrNames = rewriteClassAttrRefsConfigAttrNames config
    expr = rewriteClassAttrRefsConfigExpr config

    rewrite childExpr =
      rewriteClassAttrRefs
        RewriteClassAttrRefsConfig
          { rewriteClassAttrRefsConfigClassName = className,
            rewriteClassAttrRefsConfigAttrNames = attrNames,
            rewriteClassAttrRefsConfigExpr = childExpr
          }

    rewriteClause clause =
      clause
        { listComprehensionClauseIter = rewrite (listComprehensionClauseIter clause),
          listComprehensionClauseConds = map rewrite (listComprehensionClauseConds clause)
        }
