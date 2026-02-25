module PythonHS.Evaluator.EvalStatements (evalStatements) where

import Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.Lexer.Position (Position)
import PythonHS.AST.BinaryOperator (BinaryOperator (..))
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalForStmt (evalForStmt)
import PythonHS.Evaluator.EvalWhileStmt (evalWhileStmt)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (BreakValue, ContinueValue, DictValue, IntValue, ListValue, NoneValue, StringValue))
import PythonHS.Evaluator.ValueToOutput (valueToOutput)

-- Evaluate a list of statements, threading environment, function-env and accumulated output.
-- The Maybe (Value, Position) in the result indicates a `return` value and its source position (if any).

evalStatements :: Env -> FuncEnv -> [String] -> [Stmt] -> Either String (Env, FuncEnv, [String], Maybe (Value, Position))
evalStatements env fenv outputs [] = Right (env, fenv, outputs, Nothing)
evalStatements env fenv outputs (stmt : rest) = case stmt of
  AssignStmt name expr _ -> do
    (val, exprOuts, envAfterExpr) <- evalExpr env fenv expr
    let env' = Map.insert name val envAfterExpr
    evalStatements env' fenv (outputs ++ exprOuts) rest

  AddAssignStmt name expr pos -> do
    current <-
      case Map.lookup name env of
        Just v -> Right v
        Nothing -> Left $ "Name error: undefined identifier " ++ name ++ " at " ++ showPos pos
    (rhs, exprOuts, envAfterExpr) <- evalExpr env fenv expr
    newValue <-
      case (current, rhs) of
        (IntValue li, IntValue ri) -> Right (IntValue (li + ri))
        (StringValue ls, StringValue rs) -> Right (StringValue (ls ++ rs))
        _ -> Left $ "Type error: + expects int+int or string+string at " ++ showPos pos
    let env' = Map.insert name newValue envAfterExpr
    evalStatements env' fenv (outputs ++ exprOuts) rest

  SubAssignStmt name expr pos -> do
    current <-
      case Map.lookup name env of
        Just v -> Right v
        Nothing -> Left $ "Name error: undefined identifier " ++ name ++ " at " ++ showPos pos
    (rhs, exprOuts, envAfterExpr) <- evalExpr env fenv expr
    leftInt <- expectInt "-=" pos current
    rightInt <- expectInt "-=" pos rhs
    let env' = Map.insert name (IntValue (leftInt - rightInt)) envAfterExpr
    evalStatements env' fenv (outputs ++ exprOuts) rest

  MulAssignStmt name expr pos -> do
    current <-
      case Map.lookup name env of
        Just v -> Right v
        Nothing -> Left $ "Name error: undefined identifier " ++ name ++ " at " ++ showPos pos
    (rhs, exprOuts, envAfterExpr) <- evalExpr env fenv expr
    leftInt <- expectInt "*=" pos current
    rightInt <- expectInt "*=" pos rhs
    let env' = Map.insert name (IntValue (leftInt * rightInt)) envAfterExpr
    evalStatements env' fenv (outputs ++ exprOuts) rest

  DivAssignStmt name expr pos -> do
    current <-
      case Map.lookup name env of
        Just v -> Right v
        Nothing -> Left $ "Name error: undefined identifier " ++ name ++ " at " ++ showPos pos
    (rhs, exprOuts, envAfterExpr) <- evalExpr env fenv expr
    leftInt <- expectInt "/=" pos current
    rightInt <- expectInt "/=" pos rhs
    if rightInt == 0
      then Left $ "Value error: division by zero at " ++ showPos pos
      else do
        let env' = Map.insert name (IntValue (leftInt `div` rightInt)) envAfterExpr
        evalStatements env' fenv (outputs ++ exprOuts) rest

  ModAssignStmt name expr pos -> do
    current <-
      case Map.lookup name env of
        Just v -> Right v
        Nothing -> Left $ "Name error: undefined identifier " ++ name ++ " at " ++ showPos pos
    (rhs, exprOuts, envAfterExpr) <- evalExpr env fenv expr
    leftInt <- expectInt "%=" pos current
    rightInt <- expectInt "%=" pos rhs
    if rightInt == 0
      then Left $ "Value error: modulo by zero at " ++ showPos pos
      else do
        let env' = Map.insert name (IntValue (leftInt `mod` rightInt)) envAfterExpr
        evalStatements env' fenv (outputs ++ exprOuts) rest

  FloorDivAssignStmt name expr pos -> do
    current <-
      case Map.lookup name env of
        Just v -> Right v
        Nothing -> Left $ "Name error: undefined identifier " ++ name ++ " at " ++ showPos pos
    (rhs, exprOuts, envAfterExpr) <- evalExpr env fenv expr
    leftInt <- expectInt "//=" pos current
    rightInt <- expectInt "//=" pos rhs
    if rightInt == 0
      then Left $ "Value error: division by zero at " ++ showPos pos
      else do
        let env' = Map.insert name (IntValue (leftInt `div` rightInt)) envAfterExpr
        evalStatements env' fenv (outputs ++ exprOuts) rest

  PrintStmt expr _ ->
    case expr of
      StringExpr s _ -> evalStatements env fenv (outputs ++ [s]) rest
      _ -> do
        (val, exprOuts, envAfterExpr) <- evalExpr env fenv expr
        evalStatements envAfterExpr fenv (outputs ++ exprOuts ++ [valueToOutput val]) rest

  ReturnStmt expr pos -> do
    (val, exprOuts, envAfterExpr) <- evalExpr env fenv expr
    -- propagate return immediately, include any outputs produced evaluating the expression
    Right (envAfterExpr, fenv, outputs ++ exprOuts, Just (val, pos))

  BreakStmt pos -> Right (env, fenv, outputs, Just (BreakValue, pos))

  ContinueStmt pos -> Right (env, fenv, outputs, Just (ContinueValue, pos))

  GlobalStmt _ _ -> evalStatements env fenv outputs rest

  PassStmt _ -> evalStatements env fenv outputs rest

  IfStmt cond thenBranch maybeElse _ -> do
    (condVal, condOuts, envAfterCond) <- evalExpr env fenv cond
    condNum <- expectTruthy "if condition" (exprPos cond) condVal
    if condNum /= 0
      then do
        (envThen, fenvThen, outputsThen, ret) <- evalStatements envAfterCond fenv [] thenBranch
        case ret of
          Just (BreakValue, _) -> Right (envThen, fenvThen, outputs ++ condOuts ++ outputsThen, ret)
          Just (ContinueValue, _) -> Right (envThen, fenvThen, outputs ++ condOuts ++ outputsThen, ret)
          Just _ -> Right (envThen, fenvThen, outputs ++ condOuts ++ outputsThen, ret)
          Nothing -> evalStatements envThen fenvThen (outputs ++ condOuts ++ outputsThen) rest
      else case maybeElse of
        Just elseBranch -> do
          (envElse, fenvElse, outputsElse, ret) <- evalStatements envAfterCond fenv [] elseBranch
          case ret of
            Just (BreakValue, _) -> Right (envElse, fenvElse, outputs ++ condOuts ++ outputsElse, ret)
            Just (ContinueValue, _) -> Right (envElse, fenvElse, outputs ++ condOuts ++ outputsElse, ret)
            Just _ -> Right (envElse, fenvElse, outputs ++ condOuts ++ outputsElse, ret)
            Nothing -> evalStatements envElse fenvElse (outputs ++ condOuts ++ outputsElse) rest
        Nothing -> evalStatements envAfterCond fenv (outputs ++ condOuts) rest

  WhileStmt cond body whilePos ->
    evalWhileStmt evalStatements evalExpr env fenv outputs cond body whilePos rest

  ForStmt name iterExpr body forPos ->
    evalForStmt evalStatements evalExpr env fenv outputs name iterExpr body forPos rest

  FunctionDefStmt name params body _ -> do
    let fenv' = Map.insert name (params, body) fenv
    evalStatements env fenv' outputs rest

-- evalExpr and evalArgs are local helpers (keeps a single top-level function in this file)
  where
    -- Expressions evaluate to (Value, [String]) where the second element is any outputs
    -- produced while evaluating the expression (e.g. from nested function calls).
    evalExpr :: Env -> FuncEnv -> Expr -> Either String (Value, [String], Env)
    evalExpr env'' _ (IntegerExpr n _) = Right (IntValue n, [], env'')
    evalExpr env'' _ (StringExpr s _) = Right (StringValue s, [], env'')
    evalExpr env'' _ (NoneExpr _) = Right (NoneValue, [], env'')
    evalExpr env' fenv' (ListExpr exprs _) = do
      (vals, outs, envAfterArgs) <- evalArgs env' fenv' exprs
      Right (ListValue vals, outs, envAfterArgs)
    evalExpr env' fenv' (DictExpr entries _) = do
      (pairs, outs, envAfterEntries) <- evalDictEntries env' fenv' entries
      Right (DictValue pairs, outs, envAfterEntries)
    evalExpr env' _ (IdentifierExpr name pos) =
      case Map.lookup name env' of
        Just v -> Right (v, [], env')
        Nothing -> Left $ "Name error: undefined identifier " ++ name ++ " at " ++ showPos pos

    evalExpr env' fenv' (UnaryMinusExpr e pos) = do
      (v, outs, envAfterExpr) <- evalExpr env' fenv' e
      case v of
        IntValue n -> Right (IntValue (negate n), outs, envAfterExpr)
        _ -> Left $ "Type error: unary - expects int at " ++ showPos pos

    evalExpr env' fenv' (NotExpr e _) = do
      (v, outs, envAfterExpr) <- evalExpr env' fenv' e
      nv <- expectTruthy "not" (exprPos e) v
      Right (IntValue (if nv == 0 then 1 else 0), outs, envAfterExpr)

    evalExpr env' fenv' (BinaryExpr op l r pos) = case op of
      AddOperator -> do
        (lv, louts, envAfterLeft) <- evalExpr env' fenv' l
        (rv, routs, envAfterRight) <- evalExpr envAfterLeft fenv' r
        case (lv, rv) of
          (IntValue li, IntValue ri) -> Right (IntValue (li + ri), louts ++ routs, envAfterRight)
          (StringValue ls, StringValue rs) -> Right (StringValue (ls ++ rs), louts ++ routs, envAfterRight)
          _ -> Left $ "Type error: + expects int+int or string+string at " ++ showPos pos

      SubtractOperator -> do
        (lv, louts, envAfterLeft) <- evalExpr env' fenv' l
        (rv, routs, envAfterRight) <- evalExpr envAfterLeft fenv' r
        li <- expectInt "-" pos lv
        ri <- expectInt "-" pos rv
        Right (IntValue (li - ri), louts ++ routs, envAfterRight)

      MultiplyOperator -> do
        (lv, louts, envAfterLeft) <- evalExpr env' fenv' l
        (rv, routs, envAfterRight) <- evalExpr envAfterLeft fenv' r
        li <- expectInt "*" pos lv
        ri <- expectInt "*" pos rv
        Right (IntValue (li * ri), louts ++ routs, envAfterRight)

      DivideOperator -> do
        (lv, louts, envAfterLeft) <- evalExpr env' fenv' l
        (rv, routs, envAfterRight) <- evalExpr envAfterLeft fenv' r
        li <- expectInt "/" pos lv
        ri <- expectInt "/" pos rv
        if ri == 0
          then Left $ "Value error: division by zero at " ++ showPos pos
          else Right (IntValue (li `div` ri), louts ++ routs, envAfterRight)

      FloorDivideOperator -> do
        (lv, louts, envAfterLeft) <- evalExpr env' fenv' l
        (rv, routs, envAfterRight) <- evalExpr envAfterLeft fenv' r
        li <- expectInt "//" pos lv
        ri <- expectInt "//" pos rv
        if ri == 0
          then Left $ "Value error: division by zero at " ++ showPos pos
          else Right (IntValue (li `div` ri), louts ++ routs, envAfterRight)

      ModuloOperator -> do
        (lv, louts, envAfterLeft) <- evalExpr env' fenv' l
        (rv, routs, envAfterRight) <- evalExpr envAfterLeft fenv' r
        li <- expectInt "%" pos lv
        ri <- expectInt "%" pos rv
        if ri == 0
          then Left $ "Value error: modulo by zero at " ++ showPos pos
          else Right (IntValue (li `mod` ri), louts ++ routs, envAfterRight)

      EqOperator -> do
        (lv, louts, envAfterLeft) <- evalExpr env' fenv' l
        (rv, routs, envAfterRight) <- evalExpr envAfterLeft fenv' r
        Right (IntValue (if lv == rv then 1 else 0), louts ++ routs, envAfterRight)

      NotEqOperator -> do
        (lv, louts, envAfterLeft) <- evalExpr env' fenv' l
        (rv, routs, envAfterRight) <- evalExpr envAfterLeft fenv' r
        Right (IntValue (if lv /= rv then 1 else 0), louts ++ routs, envAfterRight)

      LtOperator -> do
        (lv, louts, envAfterLeft) <- evalExpr env' fenv' l
        (rv, routs, envAfterRight) <- evalExpr envAfterLeft fenv' r
        li <- expectInt "<" pos lv
        ri <- expectInt "<" pos rv
        Right (IntValue (if li < ri then 1 else 0), louts ++ routs, envAfterRight)

      GtOperator -> do
        (lv, louts, envAfterLeft) <- evalExpr env' fenv' l
        (rv, routs, envAfterRight) <- evalExpr envAfterLeft fenv' r
        li <- expectInt ">" pos lv
        ri <- expectInt ">" pos rv
        Right (IntValue (if li > ri then 1 else 0), louts ++ routs, envAfterRight)

      LteOperator -> do
        (lv, louts, envAfterLeft) <- evalExpr env' fenv' l
        (rv, routs, envAfterRight) <- evalExpr envAfterLeft fenv' r
        li <- expectInt "<=" pos lv
        ri <- expectInt "<=" pos rv
        Right (IntValue (if li <= ri then 1 else 0), louts ++ routs, envAfterRight)

      GteOperator -> do
        (lv, louts, envAfterLeft) <- evalExpr env' fenv' l
        (rv, routs, envAfterRight) <- evalExpr envAfterLeft fenv' r
        li <- expectInt ">=" pos lv
        ri <- expectInt ">=" pos rv
        Right (IntValue (if li >= ri then 1 else 0), louts ++ routs, envAfterRight)

      AndOperator -> do
        (lv, louts, envAfterLeft) <- evalExpr env' fenv' l
        li <- expectTruthy "and" pos lv
        if li == 0
          then Right (IntValue 0, louts, envAfterLeft)
          else do
            (rv, routs, envAfterRight) <- evalExpr envAfterLeft fenv' r
            ri <- expectTruthy "and" pos rv
            Right (IntValue (if ri /= 0 then 1 else 0), louts ++ routs, envAfterRight)

      OrOperator -> do
        (lv, louts, envAfterLeft) <- evalExpr env' fenv' l
        li <- expectTruthy "or" pos lv
        if li /= 0
          then Right (IntValue 1, louts, envAfterLeft)
          else do
            (rv, routs, envAfterRight) <- evalExpr envAfterLeft fenv' r
            ri <- expectTruthy "or" pos rv
            Right (IntValue (if ri /= 0 then 1 else 0), louts ++ routs, envAfterRight)

    evalExpr env' fenv' (CallExpr fname args pos) = case Map.lookup fname fenv' of
      _ | fname == "len" -> do
        (argVals, argOuts, envAfterArgs) <- evalArgs env' fenv' args
        case argVals of
          [StringValue s] -> Right (IntValue (length s), argOuts, envAfterArgs)
          [ListValue vals] -> Right (IntValue (length vals), argOuts, envAfterArgs)
          [_] -> Left $ "Type error: len expects string or list at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling len at " ++ showPos pos
      _ | fname == "bool" -> do
        (argVals, argOuts, envAfterArgs) <- evalArgs env' fenv' args
        case argVals of
          [IntValue n] -> Right (IntValue (if n == 0 then 0 else 1), argOuts, envAfterArgs)
          [NoneValue] -> Right (IntValue 0, argOuts, envAfterArgs)
          [StringValue s] -> Right (IntValue (if null s then 0 else 1), argOuts, envAfterArgs)
          [ListValue vals] -> Right (IntValue (if null vals then 0 else 1), argOuts, envAfterArgs)
          [DictValue pairs] -> Right (IntValue (if null pairs then 0 else 1), argOuts, envAfterArgs)
          _ -> Left $ "Argument count mismatch when calling bool at " ++ showPos pos
      _ | fname == "range" -> do
        (argVals, argOuts, envAfterArgs) <- evalArgs env' fenv' args
        case argVals of
          [IntValue n] -> Right (ListValue (map IntValue (rangeOne n)), argOuts, envAfterArgs)
          [IntValue start, IntValue stop] -> Right (ListValue (map IntValue (rangeWithStep start stop 1)), argOuts, envAfterArgs)
          [IntValue start, IntValue stop, IntValue step] ->
            if step == 0
              then Left $ "Value error: range step must not be zero at " ++ showPos pos
              else Right (ListValue (map IntValue (rangeWithStep start stop step)), argOuts, envAfterArgs)
          [_] -> Left $ "Type error: range expects int at " ++ showPos pos
          [_, _] -> Left $ "Type error: range expects int arguments at " ++ showPos pos
          [_, _, _] -> Left $ "Type error: range expects int arguments at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling range at " ++ showPos pos
      _ | fname == "append" -> do
        (argVals, argOuts, envAfterArgs) <- evalArgs env' fenv' args
        case argVals of
          [ListValue vals, value] -> Right (ListValue (vals ++ [value]), argOuts, envAfterArgs)
          [_, _] -> Left $ "Type error: append expects list as first argument at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling append at " ++ showPos pos
      _ | fname == "sort" -> do
        (argVals, argOuts, envAfterArgs) <- evalArgs env' fenv' args
        case argVals of
          [ListValue vals] ->
            case intValues vals of
              Just ns -> Right (ListValue (map IntValue (sort ns)), argOuts, envAfterArgs)
              Nothing -> Left $ "Type error: sort expects list of int at " ++ showPos pos
          [_] -> Left $ "Type error: sort expects list as first argument at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling sort at " ++ showPos pos
      _ | fname == "reverse" -> do
        (argVals, argOuts, envAfterArgs) <- evalArgs env' fenv' args
        case argVals of
          [ListValue vals] -> Right (ListValue (reverse vals), argOuts, envAfterArgs)
          [_] -> Left $ "Type error: reverse expects list as first argument at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling reverse at " ++ showPos pos
      _ | fname == "remove" -> do
        (argVals, argOuts, envAfterArgs) <- evalArgs env' fenv' args
        case argVals of
          [ListValue vals, target] ->
            case removeFirstValue vals target of
              Just newVals -> Right (ListValue newVals, argOuts, envAfterArgs)
              Nothing -> Left $ "Value error: remove value not found at " ++ showPos pos
          [_, _] -> Left $ "Type error: remove expects list as first argument at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling remove at " ++ showPos pos
      _ | fname == "insert" -> do
        (argVals, argOuts, envAfterArgs) <- evalArgs env' fenv' args
        case argVals of
          [ListValue vals, IntValue index, value] -> Right (ListValue (insertAtIndex vals index value), argOuts, envAfterArgs)
          [ListValue _, _, _] -> Left $ "Type error: insert expects int index at " ++ showPos pos
          [_, _, _] -> Left $ "Type error: insert expects list as first argument at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling insert at " ++ showPos pos
      _ | fname == "pop" -> do
        (argVals, argOuts, envAfterArgs) <- evalArgs env' fenv' args
        case argVals of
          [ListValue []] -> Left $ "Value error: pop from empty list at " ++ showPos pos
          [ListValue vals] -> Right (last vals, argOuts, envAfterArgs)
          [DictValue pairs, key] ->
            case lookupDictValue pairs key of
              Just value -> Right (value, argOuts, envAfterArgs)
              Nothing -> Left $ "Key not found in pop at " ++ showPos pos
          [DictValue pairs, key, defaultValue] ->
            case lookupDictValue pairs key of
              Just value -> Right (value, argOuts, envAfterArgs)
              Nothing -> Right (defaultValue, argOuts, envAfterArgs)
          [_] -> Left $ "Type error: pop expects list at " ++ showPos pos
          [ListValue _, _] -> Left $ "Argument count mismatch when calling pop at " ++ showPos pos
          [ListValue _, _, _] -> Left $ "Argument count mismatch when calling pop at " ++ showPos pos
          [_, _] -> Left $ "Type error: pop expects dict as first argument at " ++ showPos pos
          [_, _, _] -> Left $ "Type error: pop expects dict as first argument at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling pop at " ++ showPos pos
      _ | fname == "clear" -> do
        (argVals, argOuts, envAfterArgs) <- evalArgs env' fenv' args
        case argVals of
          [ListValue _] -> Right (ListValue [], argOuts, envAfterArgs)
          [DictValue _] -> Right (DictValue [], argOuts, envAfterArgs)
          [_] -> Left $ "Type error: clear expects list or dict at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling clear at " ++ showPos pos
      _ | fname == "keys" -> do
        (argVals, argOuts, envAfterArgs) <- evalArgs env' fenv' args
        case argVals of
          [DictValue pairs] -> Right (ListValue (map fst pairs), argOuts, envAfterArgs)
          [_] -> Left $ "Type error: keys expects dict at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling keys at " ++ showPos pos
      _ | fname == "get" -> do
        (argVals, argOuts, envAfterArgs) <- evalArgs env' fenv' args
        case argVals of
          [DictValue pairs, key] ->
            case lookupDictValue pairs key of
              Just value -> Right (value, argOuts, envAfterArgs)
              Nothing -> Left $ "Key not found in get at " ++ showPos pos
          [DictValue pairs, key, defaultValue] ->
            case lookupDictValue pairs key of
              Just value -> Right (value, argOuts, envAfterArgs)
              Nothing -> Right (defaultValue, argOuts, envAfterArgs)
          [_, _] -> Left $ "Type error: get expects dict as first argument at " ++ showPos pos
          [_, _, _] -> Left $ "Type error: get expects dict as first argument at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling get at " ++ showPos pos
      _ | fname == "update" -> do
        (argVals, argOuts, envAfterArgs) <- evalArgs env' fenv' args
        case argVals of
          [DictValue pairs, key, value] -> Right (DictValue (updateDictValue pairs key value), argOuts, envAfterArgs)
          [_, _, _] -> Left $ "Type error: update expects dict as first argument at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling update at " ++ showPos pos
      _ | fname == "setdefault" -> do
        (argVals, argOuts, envAfterArgs) <- evalArgs env' fenv' args
        case argVals of
          [DictValue pairs, key, defaultValue] -> Right (DictValue (setDefaultDictValue pairs key defaultValue), argOuts, envAfterArgs)
          [_, _, _] -> Left $ "Type error: setdefault expects dict as first argument at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling setdefault at " ++ showPos pos
      _ | fname == "values" -> do
        (argVals, argOuts, envAfterArgs) <- evalArgs env' fenv' args
        case argVals of
          [DictValue pairs] -> Right (ListValue (map snd pairs), argOuts, envAfterArgs)
          [_] -> Left $ "Type error: values expects dict at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling values at " ++ showPos pos
      _ | fname == "items" -> do
        (argVals, argOuts, envAfterArgs) <- evalArgs env' fenv' args
        case argVals of
          [DictValue pairs] -> Right (ListValue (map pairToList pairs), argOuts, envAfterArgs)
          [_] -> Left $ "Type error: items expects dict at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling items at " ++ showPos pos
      Nothing -> Left $ "Name error: undefined function " ++ fname ++ " at " ++ showPos pos
      Just (params, body) -> do
        (argVals, argOuts, envAfterArgs) <- evalArgs env' fenv' args
        if length params /= length argVals
          then Left $ "Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos
          else do
            let globalNames = collectGlobalNames body
            let localEnv = Map.union (Map.fromList (zip params argVals)) envAfterArgs
            (finalEnv, _finalFenv, bodyOuts, ret) <- evalStatements localEnv fenv' [] body
            let retVal = case ret of
                  Just (v, _) -> v
                  Nothing -> IntValue 0
            let propagatedEnv = applyGlobalWrites envAfterArgs finalEnv globalNames
            Right (retVal, argOuts ++ bodyOuts, propagatedEnv)

    -- Evaluate argument expressions left-to-right, collecting values and any outputs.
    evalArgs :: Env -> FuncEnv -> [Expr] -> Either String ([Value], [String], Env)
    evalArgs env'' fenv'' = foldl go (Right ([], [], env''))
      where
        go acc expr = do
          (vals, outs, envCurrent) <- acc
          (value, exprOuts, envNext) <- evalExpr envCurrent fenv'' expr
          Right (vals ++ [value], outs ++ exprOuts, envNext)

    evalDictEntries :: Env -> FuncEnv -> [(Expr, Expr)] -> Either String ([(Value, Value)], [String], Env)
    evalDictEntries env'' _ [] = Right ([], [], env'')
    evalDictEntries env'' fenv'' ((keyExpr, valueExpr) : restEntries) = do
      (keyVal, keyOuts, envAfterKey) <- evalExpr env'' fenv'' keyExpr
      (valueVal, valueOuts, envAfterValue) <- evalExpr envAfterKey fenv'' valueExpr
      (restVals, restOuts, envAfterRest) <- evalDictEntries envAfterValue fenv'' restEntries
      Right ((keyVal, valueVal) : restVals, keyOuts ++ valueOuts ++ restOuts, envAfterRest)

    collectGlobalNames :: [Stmt] -> [String]
    collectGlobalNames stmts = nub (concatMap goStmt stmts)
      where
        goStmt :: Stmt -> [String]
        goStmt (GlobalStmt name _) = [name]
        goStmt (IfStmt _ thenBranch maybeElse _) =
          concatMap goStmt thenBranch ++ maybe [] (concatMap goStmt) maybeElse
        goStmt (WhileStmt _ body _) = concatMap goStmt body
        goStmt (ForStmt _ _ body _) = concatMap goStmt body
        goStmt (FunctionDefStmt _ _ _ _) = []
        goStmt _ = []

    applyGlobalWrites :: Env -> Env -> [String] -> Env
    applyGlobalWrites outerEnv finalLocalEnv names =
      foldl applyOne outerEnv names
      where
        applyOne accEnv name =
          case Map.lookup name finalLocalEnv of
            Just value -> Map.insert name value accEnv
            Nothing -> accEnv

    lookupDictValue :: [(Value, Value)] -> Value -> Maybe Value
    lookupDictValue [] _ = Nothing
    lookupDictValue ((k, v) : restPairs) target
      | k == target = Just v
      | otherwise = lookupDictValue restPairs target

    updateDictValue :: [(Value, Value)] -> Value -> Value -> [(Value, Value)]
    updateDictValue [] key value = [(key, value)]
    updateDictValue ((k, v) : restPairs) key value
      | k == key = (k, value) : restPairs
      | otherwise = (k, v) : updateDictValue restPairs key value

    setDefaultDictValue :: [(Value, Value)] -> Value -> Value -> [(Value, Value)]
    setDefaultDictValue pairs key defaultValue =
      case lookupDictValue pairs key of
        Just _ -> pairs
        Nothing -> pairs ++ [(key, defaultValue)]

    removeFirstValue :: [Value] -> Value -> Maybe [Value]
    removeFirstValue [] _ = Nothing
    removeFirstValue (v : restVals) target
      | v == target = Just restVals
      | otherwise = fmap (v :) (removeFirstValue restVals target)

    intValues :: [Value] -> Maybe [Int]
    intValues [] = Just []
    intValues (IntValue n : restVals) = fmap (n :) (intValues restVals)
    intValues (_ : _) = Nothing

    insertAtIndex :: [Value] -> Int -> Value -> [Value]
    insertAtIndex values index value =
      let clampedIndex = max 0 (min index (length values))
          (leftValues, rightValues) = splitAt clampedIndex values
       in leftValues ++ (value : rightValues)

    pairToList :: (Value, Value) -> Value
    pairToList (k, v) = ListValue [k, v]

    rangeOne :: Int -> [Int]
    rangeOne n
      | n <= 0 = []
      | otherwise = [0 .. n - 1]

    rangeWithStep :: Int -> Int -> Int -> [Int]
    rangeWithStep start stop step
      | step > 0 = takeWhile (< stop) [start, start + step ..]
      | step < 0 = takeWhile (> stop) [start, start + step ..]
      | otherwise = []

    exprPos :: Expr -> Position
    exprPos (IntegerExpr _ pos) = pos
    exprPos (StringExpr _ pos) = pos
    exprPos (NoneExpr pos) = pos
    exprPos (ListExpr _ pos) = pos
    exprPos (DictExpr _ pos) = pos
    exprPos (IdentifierExpr _ pos) = pos
    exprPos (UnaryMinusExpr _ pos) = pos
    exprPos (NotExpr _ pos) = pos
    exprPos (BinaryExpr _ _ _ pos) = pos
    exprPos (CallExpr _ _ pos) = pos

    expectInt :: String -> Position -> Value -> Either String Int
    expectInt _ _ (IntValue n) = Right n
    expectInt _ _ NoneValue = Right 0
    expectInt context pos _ = Left $ "Type error: expected int in " ++ context ++ " at " ++ showPos pos

    expectTruthy :: String -> Position -> Value -> Either String Int
    expectTruthy _ _ (IntValue n) = Right (if n == 0 then 0 else 1)
    expectTruthy _ _ NoneValue = Right 0
    expectTruthy _ _ (StringValue s) = Right (if null s then 0 else 1)
    expectTruthy _ _ (ListValue vals) = Right (if null vals then 0 else 1)
    expectTruthy _ _ (DictValue pairs) = Right (if null pairs then 0 else 1)
    expectTruthy context pos _ = Left $ "Type error: expected int in " ++ context ++ " at " ++ showPos pos
