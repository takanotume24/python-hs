module PythonHS.VM.CompileDefaults (compileDefaults) where

import PythonHS.AST.Expr (Expr)
import PythonHS.VM.CompileDefaultsConfig (CompileDefaultsConfig (..))
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.Instruction (Instruction (ReturnTop))

compileDefaults :: CompileDefaultsConfig -> Either String ([(String, [Instruction])], Int)
compileDefaults config =
  let compileExpr = compileDefaultsCompileExpr config
      defaults = compileDefaultsDefaults config
   in case defaults of
        [] -> Right ([], 0)
        (name, expr) : rest -> do
          result <- compileExpr 0 expr
          let compiledDefault = compileExprResultCode result ++ [ReturnTop]
          (restDefaults, _) <- compileDefaults config {compileDefaultsCompileExpr = compileExpr, compileDefaultsDefaults = rest}
          Right ((name, compiledDefault) : restDefaults, compileExprResultEndIndex result + 1)
