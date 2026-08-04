module PythonHS.VM.CompileFunctionDefStmt (compileFunctionDefStmt) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Evaluator.Value (Value (..))
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
   in do
        defaultsResult <- compileDefaults CompileDefaultsConfig {compileDefaultsCompileExpr = compileExpr, compileDefaultsDefaults = defaults}
        functionCode <-
          if hasYieldInStmts body
            then do
              bodyResult <- compileStatements 2 True Nothing body
              pure ([PushConst {pushConstValue = ListValue {listValueItems = []}}, StoreName {storeNameName = "__yield_acc__"}] ++ compileExprResultCode bodyResult ++ [LoadName {loadNameName = "__yield_acc__", loadNamePos = posDef}, ReturnTop])
            else do
              bodyResult <- compileStatements 0 True Nothing body
              pure (compileExprResultCode bodyResult ++ [PushConst {pushConstValue = IntValue {intValue = 0}}, ReturnTop])
        pure FunctionDefStmtResult {functionDefStmtResultCode = functionCode, functionDefStmtResultDefaults = compileDefaultsResultDefaults defaultsResult}
