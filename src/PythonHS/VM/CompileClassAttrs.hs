module PythonHS.VM.CompileClassAttrs (compileClassAttrs) where

import PythonHS.VM.ClassAttrItem (ClassAttrItem (..))
import PythonHS.VM.CompileClassAttrsConfig (CompileClassAttrsConfig (..))
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.Instruction (Instruction (..))
import PythonHS.VM.RewriteClassAttrRefs (rewriteClassAttrRefs)
import PythonHS.VM.RewriteClassAttrRefsConfig (RewriteClassAttrRefsConfig (..))

compileClassAttrs :: CompileClassAttrsConfig -> Either String CompileExprResult
compileClassAttrs config = compileAttrsAt (compileClassAttrsBaseIndex config) (compileClassAttrsAttrs config)
  where
    compileExpr = compileClassAttrsCompileExpr config
    className = compileClassAttrsClassName config
    attrNames = map classAttrItemName (compileClassAttrsAttrs config)

    rewriteExpr attrExpr =
      rewriteClassAttrRefs
        RewriteClassAttrRefsConfig
          { rewriteClassAttrRefsConfigClassName = className,
            rewriteClassAttrRefsConfigAttrNames = attrNames,
            rewriteClassAttrRefsConfigExpr = attrExpr
          }

    compileAttrsAt idx [] = Right CompileExprResult {compileExprResultCode = [], compileExprResultEndIndex = idx}
    compileAttrsAt idx (attrItem : rest) = do
      let attrName = classAttrItemName attrItem
          attrExpr = rewriteExpr (classAttrItemExpr attrItem)
      exprResult <- compileExpr idx attrExpr
      let attrInstr = StoreClassAttr {storeClassAttrClassName = className, storeClassAttrAttrName = attrName}
          attrResult = CompileExprResult {compileExprResultCode = compileExprResultCode exprResult ++ [attrInstr], compileExprResultEndIndex = compileExprResultEndIndex exprResult + 1}
      restResult <- compileAttrsAt (compileExprResultEndIndex attrResult) rest
      pure CompileExprResult {compileExprResultCode = compileExprResultCode attrResult ++ compileExprResultCode restResult, compileExprResultEndIndex = compileExprResultEndIndex restResult}
