module PythonHS.VM.CompileDefaults (compileDefaults) where

import PythonHS.AST.FunctionDefault (FunctionDefault (..))
import PythonHS.VM.CompileDefaultsConfig (CompileDefaultsConfig (..))
import PythonHS.VM.CompileDefaultsResult (CompileDefaultsResult (..))
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.CompiledDefaultCode (CompiledDefaultCode (..))
import PythonHS.VM.Instruction (Instruction (ReturnTop))

compileDefaults :: CompileDefaultsConfig -> Either String CompileDefaultsResult
compileDefaults config =
  let compileExpr = compileDefaultsCompileExpr config
      defaults = compileDefaultsDefaults config
   in case defaults of
        [] -> Right CompileDefaultsResult {compileDefaultsResultDefaults = [], compileDefaultsResultEndIndex = 0}
        FunctionDefault {functionDefaultParam = name, functionDefaultExpr = expr} : rest -> do
          result <- compileExpr 0 expr
          let compiledDefault = compileExprResultCode result ++ [ReturnTop]
          restResult <- compileDefaults config {compileDefaultsCompileExpr = compileExpr, compileDefaultsDefaults = rest}
          Right CompileDefaultsResult {compileDefaultsResultDefaults = CompiledDefaultCode {compiledDefaultCodeParam = name, compiledDefaultCodeInstructions = compiledDefault} : compileDefaultsResultDefaults restResult, compileDefaultsResultEndIndex = compileExprResultEndIndex result + 1}
