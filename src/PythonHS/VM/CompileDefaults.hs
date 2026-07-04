module PythonHS.VM.CompileDefaults (compileDefaults) where

import PythonHS.AST.Expr (Expr)
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.Instruction (Instruction (ReturnTop))

compileDefaults :: (Int -> Expr -> Either String CompileExprResult) -> [(String, Expr)] -> Either String ([(String, [Instruction])], Int)
compileDefaults compileExprAt defaults =
  case defaults of
    [] -> Right ([], 0)
    (name, expr) : rest -> do
      result <- compileExprAt 0 expr
      let compiledDefault = compileExprResultCode result ++ [ReturnTop]
      (restDefaults, _) <- compileDefaults compileExprAt rest
      Right ((name, compiledDefault) : restDefaults, compileExprResultEndIndex result + 1)
