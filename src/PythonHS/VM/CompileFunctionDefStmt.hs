module PythonHS.VM.CompileFunctionDefStmt (compileFunctionDefStmt) where

import Data.Set qualified as Set
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CollectFunctionLocalDecls (collectFunctionLocalDecls)
import PythonHS.VM.CollectFunctionNonlocalStmtDecls (collectFunctionNonlocalStmtDecls)
import PythonHS.VM.CompileDefaults (compileDefaults)
import PythonHS.VM.CompileDefaultsConfig (CompileDefaultsConfig (..))
import PythonHS.VM.CompileDefaultsResult (CompileDefaultsResult (..))
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.CompileFunctionDefStmtConfig (CompileFunctionDefStmtConfig (..))
import PythonHS.VM.FunctionDefStmtResult (FunctionDefStmtResult (..))
import PythonHS.VM.HasYieldInStmts (hasYieldInStmts)
import PythonHS.VM.Instruction (Instruction (..))

compileFunctionDefStmt :: CompileFunctionDefStmtConfig -> Either String FunctionDefStmtResult
compileFunctionDefStmt config =
  let compileStatements = compileFunctionDefStmtCompileStatements config
      compileExpr = compileFunctionDefStmtCompileExpr config
      posDef = compileFunctionDefStmtPosDef config
      defaults = compileFunctionDefStmtDefaults config
      body = compileFunctionDefStmtBody config
      params = compileFunctionDefStmtParams config
      globalDecls = compileFunctionDefStmtGlobalDecls config
      nonlocalDecls = collectFunctionNonlocalStmtDecls body
      localDecls = collectFunctionLocalDecls params body (Set.union globalDecls nonlocalDecls)
      sentinelInitCode = buildSentinelInit localDecls
   in do
        defaultsResult <- compileDefaults CompileDefaultsConfig {compileDefaultsCompileExpr = compileExpr, compileDefaultsDefaults = defaults}
        functionCode <-
          if hasYieldInStmts body
            then do
              bodyResult <- compileStatements (2 + length sentinelInitCode) True Nothing body
              pure (sentinelInitCode ++ [PushConst {pushConstValue = ListValue {listValueItems = []}}, StoreName {storeNameName = "__yield_acc__"}] ++ compileExprResultCode bodyResult ++ [LoadName {loadNameName = "__yield_acc__", loadNamePos = posDef}, ReturnTop])
            else do
              bodyResult <- compileStatements (length sentinelInitCode) True Nothing body
              pure (sentinelInitCode ++ compileExprResultCode bodyResult ++ [PushConst {pushConstValue = IntValue {intValue = 0}}, ReturnTop])
        pure FunctionDefStmtResult {functionDefStmtResultCode = functionCode, functionDefStmtResultDefaults = compileDefaultsResultDefaults defaultsResult}
  where
    buildSentinelInit localDecls =
      concatMap (\name -> [PushConst {pushConstValue = UnboundLocalValue {unboundLocalValueName = name}}, StoreName {storeNameName = name}]) (Set.toList localDecls)
