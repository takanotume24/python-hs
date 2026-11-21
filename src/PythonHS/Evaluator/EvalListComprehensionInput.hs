module PythonHS.Evaluator.EvalListComprehensionInput (EvalListComprehensionInput (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.ListComprehensionClause (ListComprehensionClause)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Lexer.Position (Position)

data EvalListComprehensionInput = EvalListComprehensionInput
  { evalListComprehensionInputEvalExpr :: Env -> FuncEnv -> Expr -> Either String EvalExprResult,
    evalListComprehensionInputEnv :: Env,
    evalListComprehensionInputFuncEnv :: FuncEnv,
    evalListComprehensionInputValueExpr :: Expr,
    evalListComprehensionInputClauses :: [ListComprehensionClause Expr],
    evalListComprehensionInputPos :: Position
  }
