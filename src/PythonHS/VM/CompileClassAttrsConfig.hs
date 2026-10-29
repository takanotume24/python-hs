module PythonHS.VM.CompileClassAttrsConfig (CompileClassAttrsConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.VM.ClassAttrItem (ClassAttrItem)
import PythonHS.VM.CompileExprResult (CompileExprResult)

data CompileClassAttrsConfig = CompileClassAttrsConfig
  { compileClassAttrsCompileExpr :: Int -> Expr -> Either String CompileExprResult,
    compileClassAttrsClassName :: String,
    compileClassAttrsBaseIndex :: Int,
    compileClassAttrsAttrs :: [ClassAttrItem]
  }
