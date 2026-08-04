module PythonHS.EvalProgram (evalProgram) where

import Data.Map.Strict qualified as Map
import PythonHS.AST.Program (Program (Program))
import PythonHS.Evaluator.EvalStatements (evalStatements)
import PythonHS.Evaluator.EvalStmtResult (EvalStmtResult (..))
import PythonHS.Evaluator.EvalStmtRet (EvalStmtRet (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (BreakValue, ContinueValue))

evalProgram :: Program -> Either String [String]
evalProgram (Program stmts) = do
  EvalStmtResult {evalStmtResultOutputs = outputs, evalStmtResultRet = mret} <- evalStatements Map.empty Map.empty [] stmts
  case mret of
    Just EvalStmtRet {evalStmtRetValue = BreakValue, evalStmtRetPos = pos} -> Left $ "Break outside loop at " ++ showPos pos
    Just EvalStmtRet {evalStmtRetValue = ContinueValue, evalStmtRetPos = pos} -> Left $ "Continue outside loop at " ++ showPos pos
    Just EvalStmtRet {evalStmtRetPos = pos} -> Left $ "Return outside function at " ++ showPos pos
    Nothing -> Right outputs
