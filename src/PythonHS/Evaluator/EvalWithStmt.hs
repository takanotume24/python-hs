module PythonHS.Evaluator.EvalWithStmt (evalWithStmt) where

import PythonHS.AST.WithContext (ContextManager (..))
import PythonHS.Evaluator.BindContextResult (bindContextResult)
import PythonHS.Evaluator.EnterContextManager (enterContextManager)
import PythonHS.Evaluator.EnterContextManagerInput (EnterContextManagerInput (..))
import PythonHS.Evaluator.EvalContextManager (exitContextManager, exitContextManagerWithException)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.EvalStmtResult (EvalStmtResult (..))
import PythonHS.Evaluator.EvalWithStmtConfig (EvalWithStmtConfig (..))
import PythonHS.Evaluator.EvalWithStmtInput (EvalWithStmtInput (..))
import PythonHS.Evaluator.ExitContextManagerInput (ExitContextManagerInput (..))
import PythonHS.Evaluator.ExitContextManagerWithExceptionInput (ExitContextManagerWithExceptionInput (..))
import PythonHS.Evaluator.Value (Value (IntValue))

evalWithStmt :: EvalWithStmtInput -> Either String EvalStmtResult
evalWithStmt input =
  let config = evalWithStmtInputConfig input
      env = evalWithStmtInputEnv input
      fenv = evalWithStmtInputFuncEnv input
      outputs = evalWithStmtInputOutputs input
      managerExpr = evalWithStmtInputContextManager input
      maybeVarName = evalWithStmtInputMaybeVarName input
      body = evalWithStmtInputBody input
      withPos = evalWithStmtInputPos input
      rest = evalWithStmtInputRest input
      evalStatementsFn = evalWithStmtEvalStatements config
      evalExprFn = evalWithStmtEvalExpr config
      ctxManager = ContextManager {contextManagerExpr = managerExpr, contextManagerVarName = maybeVarName, contextManagerPos = withPos}
   in do
        cmResult <- evalExprFn env fenv (contextManagerExpr ctxManager)
        let cmOuts = evalExprResultOutputs cmResult
            envAfterCM = evalExprResultEnv cmResult

        enterResult <- enterContextManager EnterContextManagerInput {enterContextManagerEvalExprFn = evalExprFn, enterContextManagerEnv = envAfterCM, enterContextManagerFuncEnv = fenv, enterContextManagerContextManager = ctxManager}
        let enterValue = evalExprResultValue enterResult
            enterOuts = evalExprResultOutputs enterResult
            envAfterEnter = evalExprResultEnv enterResult

        let envAfterBind = bindContextResult (contextManagerVarName ctxManager) enterValue envAfterEnter

        case evalStatementsFn envAfterBind fenv [] body of
          Right EvalStmtResult {evalStmtResultEnv = envAfterBody, evalStmtResultFuncEnv = fenvAfterBody, evalStmtResultOutputs = bodyOuts} -> do
            exitResult <- exitContextManager ExitContextManagerInput {exitContextManagerEvalExprFn = evalExprFn, exitContextManagerEnv = envAfterBody, exitContextManagerFuncEnv = fenvAfterBody, exitContextManagerContextManager = ctxManager}
            let exitOuts = evalExprResultOutputs exitResult
            evalStatementsFn envAfterBody fenvAfterBody (outputs ++ cmOuts ++ enterOuts ++ bodyOuts ++ exitOuts) rest
          Left err -> do
            exitResult <- exitContextManagerWithException ExitContextManagerWithExceptionInput {exitContextManagerWithExceptionEvalExprFn = evalExprFn, exitContextManagerWithExceptionEnv = envAfterEnter, exitContextManagerWithExceptionFuncEnv = fenv, exitContextManagerWithExceptionContextManager = ctxManager, exitContextManagerWithExceptionErr = err}
            let exitValue = evalExprResultValue exitResult
                exitOuts = evalExprResultOutputs exitResult
            case exitValue of
              IntValue 0 -> Left err
              _ -> evalStatementsFn envAfterEnter fenv (outputs ++ cmOuts ++ enterOuts ++ exitOuts) rest
