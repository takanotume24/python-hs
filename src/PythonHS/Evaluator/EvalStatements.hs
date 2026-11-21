module PythonHS.Evaluator.EvalStatements (evalStatements) where

import Data.Map.Strict qualified as Map
import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Evaluator.ComputeAssign (computeAssign)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalAssignStmt (evalAssignStmt)
import PythonHS.Evaluator.EvalAssignStmtInput (EvalAssignStmtInput (..))
import PythonHS.Evaluator.EvalExpr (evalExpr)
import PythonHS.Evaluator.EvalExprInput (EvalExprInput (..))
import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.EvalForStmt (evalForStmt)
import PythonHS.Evaluator.EvalForStmtConfig (EvalForStmtConfig (..))
import PythonHS.Evaluator.EvalForStmtInput (EvalForStmtInput (..))
import PythonHS.Evaluator.EvalStmtResult (EvalStmtResult (..))
import PythonHS.Evaluator.EvalStmtRet (EvalStmtRet (..))
import PythonHS.Evaluator.EvalWhileStmt (evalWhileStmt)
import PythonHS.Evaluator.EvalWhileStmtConfig (EvalWhileStmtConfig (..))
import PythonHS.Evaluator.EvalWhileStmtInput (EvalWhileStmtInput (..))
import PythonHS.Evaluator.EvalWithStmt (evalWithStmt)
import PythonHS.Evaluator.EvalWithStmtConfig (EvalWithStmtConfig (..))
import PythonHS.Evaluator.EvalWithStmtInput (EvalWithStmtInput (..))
import PythonHS.Evaluator.ExpectTruthy (expectTruthy)
import PythonHS.Evaluator.ExpectTruthyInput (ExpectTruthyInput (..))
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Evaluator.ValueToOutput (valueToOutput)
import PythonHS.Parser.ExprPos (exprPos)

evalStatements :: Env -> FuncEnv -> [String] -> [Stmt] -> Either String EvalStmtResult
evalStatements env fenv outputs [] =
  Right
    EvalStmtResult
      { evalStmtResultEnv = env,
        evalStmtResultFuncEnv = fenv,
        evalStmtResultOutputs = outputs,
        evalStmtResultRet = Nothing
      }
evalStatements env fenv outputs (stmt : rest) =
  case stmt of
    AssignStmt {assignStmtName, assignStmtValue} -> do
      (val, exprOuts, envAfterExpr) <- extractResult <$> evalExpr' assignStmtValue
      evalStatements (Map.insert assignStmtName val envAfterExpr) fenv (outputs ++ exprOuts) rest
    AssignUnpackStmt {assignUnpackStmtPos} ->
      Left $ "Runtime error: tuple unpack assignment is only supported in vm engine at " ++ showPos assignUnpackStmtPos
    AnnAssignStmt {annAssignStmtName, annAssignStmtValue} ->
      case annAssignStmtValue of
        Nothing -> evalStatements env fenv outputs rest
        Just expr -> do
          (val, exprOuts, envAfterExpr) <- extractResult <$> evalExpr' expr
          evalStatements (Map.insert annAssignStmtName val envAfterExpr) fenv (outputs ++ exprOuts) rest
    DecoratedStmt {decoratedStmtPos} -> Left $ "Runtime error: decorator is only supported in vm engine at " ++ showPos decoratedStmtPos
    AddAssignStmt {addAssignStmtName, addAssignStmtValue, addAssignStmtPos} ->
      evalAssignStmt (assignInput addAssignStmtName addAssignStmtValue addAssignStmtPos "+=")
    SubAssignStmt {subAssignStmtName, subAssignStmtValue, subAssignStmtPos} ->
      evalAssignStmt (assignInput subAssignStmtName subAssignStmtValue subAssignStmtPos "-=")
    MulAssignStmt {mulAssignStmtName, mulAssignStmtValue, mulAssignStmtPos} ->
      evalAssignStmt (assignInput mulAssignStmtName mulAssignStmtValue mulAssignStmtPos "*=")
    DivAssignStmt {divAssignStmtName, divAssignStmtValue, divAssignStmtPos} ->
      evalAssignStmt (assignInput divAssignStmtName divAssignStmtValue divAssignStmtPos "/=")
    ModAssignStmt {modAssignStmtName, modAssignStmtValue, modAssignStmtPos} ->
      evalAssignStmt (assignInput modAssignStmtName modAssignStmtValue modAssignStmtPos "%=")
    FloorDivAssignStmt {floorDivAssignStmtName, floorDivAssignStmtValue, floorDivAssignStmtPos} ->
      evalAssignStmt (assignInput floorDivAssignStmtName floorDivAssignStmtValue floorDivAssignStmtPos "//=")
    PrintStmt {printStmtValue} ->
      case printStmtValue of
        StringExpr {stringExprValue = s} -> evalStatements env fenv (outputs ++ [s]) rest
        _ -> do
          (val, exprOuts, envAfterExpr) <- extractResult <$> evalExpr' printStmtValue
          evalStatements envAfterExpr fenv (outputs ++ exprOuts ++ [valueToOutput val]) rest
    ReturnStmt {returnStmtValue, returnStmtPos} -> do
      (val, exprOuts, envAfterExpr) <- extractResult <$> evalExpr' returnStmtValue
      Right baseResult {evalStmtResultEnv = envAfterExpr, evalStmtResultOutputs = outputs ++ exprOuts, evalStmtResultRet = Just EvalStmtRet {evalStmtRetValue = val, evalStmtRetPos = returnStmtPos}}
    YieldStmt {yieldStmtPos} -> Left $ "Runtime error: yield is only supported in vm engine at " ++ showPos yieldStmtPos
    YieldFromStmt {yieldFromStmtPos} -> Left $ "Runtime error: yield from is only supported in vm engine at " ++ showPos yieldFromStmtPos
    BreakStmt {breakStmtPos} -> Right baseResult {evalStmtResultRet = Just EvalStmtRet {evalStmtRetValue = BreakValue, evalStmtRetPos = breakStmtPos}}
    ContinueStmt {continueStmtPos} -> Right baseResult {evalStmtResultRet = Just EvalStmtRet {evalStmtRetValue = ContinueValue, evalStmtRetPos = continueStmtPos}}
    GlobalStmt {} -> evalStatements env fenv outputs rest
    ImportStmt {} -> evalStatements env fenv outputs rest
    FromImportStmt {} -> evalStatements env fenv outputs rest
    TryExceptStmt {tryExceptStmtPos} -> Left $ "Runtime error: try/except is only supported in vm engine at " ++ showPos tryExceptStmtPos
    MatchStmt {matchStmtPos} -> Left $ "Runtime error: match/case is only supported in vm engine at " ++ showPos matchStmtPos
    RaiseStmt {raiseStmtExpr, raiseStmtPos} ->
      case raiseStmtExpr of
        Nothing -> Left $ "Runtime error: bare raise is only supported in vm engine at " ++ showPos raiseStmtPos
        Just expr -> do
          (val, _, _) <- extractResult <$> evalExpr' expr
          Left $ "Runtime error: " ++ valueToOutput val ++ " at " ++ showPos raiseStmtPos
    PassStmt {} -> evalStatements env fenv outputs rest
    AssertStmt {assertStmtCond, assertStmtPos} -> do
      (condVal, condOuts, envAfterCond) <- extractResult <$> evalExpr' assertStmtCond
      condNum <- expectTruthy ExpectTruthyInput {expectTruthyInputContext = "assert", expectTruthyInputPos = assertStmtPos, expectTruthyInputValue = condVal}
      if condNum /= 0
        then evalStatements envAfterCond fenv (outputs ++ condOuts) rest
        else Left $ "AssertionError: assertion failed at " ++ showPos assertStmtPos
    DeleteStmt {deleteStmtPos} -> Left $ "Runtime error: del is only supported in vm engine at " ++ showPos deleteStmtPos
    ExprStmt {exprStmtValue} -> do
      (_, exprOuts, envAfterExpr) <- extractResult <$> evalExpr' exprStmtValue
      evalStatements envAfterExpr fenv (outputs ++ exprOuts) rest
    WithStmt {withStmtContextManager, withStmtVarName, withStmtBody, withStmtPos} ->
      evalWithStmt (EvalWithStmtInput (EvalWithStmtConfig evalStatements evalExprForConfig) env fenv outputs withStmtContextManager withStmtVarName withStmtBody withStmtPos rest)
    IfStmt {ifStmtCond, ifStmtThen, ifStmtElse} -> do
      (condVal, condOuts, envAfterCond) <- extractResult <$> evalExpr' ifStmtCond
      condNum <- expectTruthy ExpectTruthyInput {expectTruthyInputContext = "if condition", expectTruthyInputPos = exprPos ifStmtCond, expectTruthyInputValue = condVal}
      if condNum /= 0
        then do
          EvalStmtResult {evalStmtResultEnv = envThen, evalStmtResultFuncEnv = fenvThen, evalStmtResultOutputs = outputsThen, evalStmtResultRet = ret} <- evalStatements envAfterCond fenv [] ifStmtThen
          case ret of
            Just _ -> Right baseResult {evalStmtResultEnv = envThen, evalStmtResultFuncEnv = fenvThen, evalStmtResultOutputs = outputs ++ condOuts ++ outputsThen, evalStmtResultRet = ret}
            Nothing -> evalStatements envThen fenvThen (outputs ++ condOuts ++ outputsThen) rest
        else case ifStmtElse of
          Just elseBranch -> do
            EvalStmtResult {evalStmtResultEnv = envElse, evalStmtResultFuncEnv = fenvElse, evalStmtResultOutputs = outputsElse, evalStmtResultRet = ret} <- evalStatements envAfterCond fenv [] elseBranch
            case ret of
              Just _ -> Right baseResult {evalStmtResultEnv = envElse, evalStmtResultFuncEnv = fenvElse, evalStmtResultOutputs = outputs ++ condOuts ++ outputsElse, evalStmtResultRet = ret}
              Nothing -> evalStatements envElse fenvElse (outputs ++ condOuts ++ outputsElse) rest
          Nothing -> evalStatements envAfterCond fenv (outputs ++ condOuts) rest
    WhileStmt {whileStmtCond, whileStmtBody, whileStmtPos} ->
      evalWhileStmt (EvalWhileStmtInput (EvalWhileStmtConfig evalStatements evalExprForConfig) env fenv outputs whileStmtCond whileStmtBody whileStmtPos rest)
    ForStmt {forStmtVar, forStmtIter, forStmtBody, forStmtPos} ->
      evalForStmt
        EvalForStmtInput
          { evalForStmtInputConfig = EvalForStmtConfig evalStatements evalExprForConfig,
            evalForStmtInputEnv = env,
            evalForStmtInputFuncEnv = fenv,
            evalForStmtInputOutputs = outputs,
            evalForStmtInputName = forStmtVar,
            evalForStmtInputIterExpr = forStmtIter,
            evalForStmtInputBody = forStmtBody,
            evalForStmtInputPos = forStmtPos,
            evalForStmtInputRest = rest
          }
    ClassDefStmt {classDefStmtPos} -> Left $ "Runtime error: class is only supported in vm engine at " ++ showPos classDefStmtPos
    FunctionDefStmt {functionDefStmtName, functionDefStmtParams, functionDefStmtBody} ->
      evalStatements env (Map.insert functionDefStmtName (functionDefStmtParams, [], functionDefStmtBody) fenv) outputs rest
    FunctionDefDefaultsStmt {functionDefDefaultsStmtName, functionDefDefaultsStmtParams, functionDefDefaultsStmtDefaults, functionDefDefaultsStmtBody} ->
      evalStatements env (Map.insert functionDefDefaultsStmtName (functionDefDefaultsStmtParams, functionDefDefaultsStmtDefaults, functionDefDefaultsStmtBody) fenv) outputs rest
  where
    extractResult r = (evalExprResultValue r, evalExprResultOutputs r, evalExprResultEnv r)

    baseResult = EvalStmtResult {evalStmtResultEnv = env, evalStmtResultFuncEnv = fenv, evalStmtResultOutputs = outputs, evalStmtResultRet = Nothing}

    evalExpr' expr' =
      evalExpr
        EvalExprInput
          { evalExprInputEvalStatementsFn = evalStatements,
            evalExprInputEnv = env,
            evalExprInputFuncEnv = fenv,
            evalExprInputExpr = expr'
          }

    evalExprForConfig env' fenv' expr' =
      evalExpr
        EvalExprInput
          { evalExprInputEvalStatementsFn = evalStatements,
            evalExprInputEnv = env',
            evalExprInputFuncEnv = fenv',
            evalExprInputExpr = expr'
          }

    assignInput name expr pos op =
      EvalAssignStmtInput
        { evalAssignStmtInputEvalExpr = evalExpr',
          evalAssignStmtInputEvalStatements = evalStatements,
          evalAssignStmtInputEnv = env,
          evalAssignStmtInputFuncEnv = fenv,
          evalAssignStmtInputOutputs = outputs,
          evalAssignStmtInputRest = rest,
          evalAssignStmtInputName = name,
          evalAssignStmtInputValueExpr = expr,
          evalAssignStmtInputPos = pos,
          evalAssignStmtInputOp = op,
          evalAssignStmtInputCompute = computeAssign
        }
