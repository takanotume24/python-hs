module PythonHS.EvalProgram (evalProgram) where

import qualified Data.Map.Strict as Map
import PythonHS.AST.Program (Program (Program))
import PythonHS.Evaluator.EvalStatements (evalStatements)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (BreakValue, ContinueValue))

evalProgram :: Program -> Either String [String]
evalProgram (Program stmts) = do
  (_, _, outputs, mret) <- evalStatements Map.empty Map.empty [] stmts
  case mret of
    Just (BreakValue, pos) -> Left $ "Break outside loop at " ++ showPos pos
    Just (ContinueValue, pos) -> Left $ "Continue outside loop at " ++ showPos pos
    Just (_, pos) -> Left $ "Return outside function at " ++ showPos pos
    Nothing -> Right outputs
