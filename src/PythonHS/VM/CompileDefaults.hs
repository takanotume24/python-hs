module PythonHS.VM.CompileDefaults (compileDefaults) where

import PythonHS.AST.Expr (Expr)
import PythonHS.VM.Instruction (Instruction (ReturnTop), Instruction)

compileDefaults :: (Int -> Expr -> Either String ([Instruction], Int)) -> [(String, Expr)] -> Either String ([(String, [Instruction])], Int)
compileDefaults compileExprAt defaults =
  case defaults of
    [] -> Right ([], 0)
    (name, expr) : rest -> do
      (exprCode, exprEnd) <- compileExprAt 0 expr
      let compiledDefault = exprCode ++ [ReturnTop]
      (restDefaults, _) <- compileDefaults compileExprAt rest
      Right ((name, compiledDefault) : restDefaults, exprEnd + 1)
