module PythonHS.VM.RewriteClassAttrRefsConfig (RewriteClassAttrRefsConfig (..)) where

import PythonHS.AST.Expr (Expr)

data RewriteClassAttrRefsConfig = RewriteClassAttrRefsConfig
  { rewriteClassAttrRefsConfigClassName :: String,
    rewriteClassAttrRefsConfigAttrNames :: [String],
    rewriteClassAttrRefsConfigExpr :: Expr
  }
