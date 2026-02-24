module PythonHS.Evaluator.EvalStatements (evalStatements) where

import Data.List (sort)
import qualified Data.Map.Strict as Map
import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.Lexer.Position (Position)
import PythonHS.AST.BinaryOperator (BinaryOperator (..))
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (BreakValue, ContinueValue, DictValue, IntValue, ListValue, NoneValue, StringValue))

-- Evaluate a list of statements, threading environment, function-env and accumulated output.
-- The Maybe (Value, Position) in the result indicates a `return` value and its source position (if any).

evalStatements :: Env -> FuncEnv -> [String] -> [Stmt] -> Either String (Env, FuncEnv, [String], Maybe (Value, Position))
evalStatements env fenv outputs [] = Right (env, fenv, outputs, Nothing)
evalStatements env fenv outputs (stmt : rest) = case stmt of
  AssignStmt name expr _ -> do
    (val, exprOuts) <- evalExpr env fenv expr
    let env' = Map.insert name val env
    evalStatements env' fenv (outputs ++ exprOuts) rest

  AddAssignStmt name expr pos -> do
    current <-
      case Map.lookup name env of
        Just v -> Right v
        Nothing -> Left $ "Name error: undefined identifier " ++ name ++ " at " ++ showPos pos
    (rhs, exprOuts) <- evalExpr env fenv expr
    newValue <-
      case (current, rhs) of
        (IntValue li, IntValue ri) -> Right (IntValue (li + ri))
        (StringValue ls, StringValue rs) -> Right (StringValue (ls ++ rs))
        _ -> Left $ "Type error: + expects int+int or string+string at " ++ showPos pos
    let env' = Map.insert name newValue env
    evalStatements env' fenv (outputs ++ exprOuts) rest

  SubAssignStmt name expr pos -> do
    current <-
      case Map.lookup name env of
        Just v -> Right v
        Nothing -> Left $ "Name error: undefined identifier " ++ name ++ " at " ++ showPos pos
    (rhs, exprOuts) <- evalExpr env fenv expr
    leftInt <- expectInt "-=" pos current
    rightInt <- expectInt "-=" pos rhs
    let env' = Map.insert name (IntValue (leftInt - rightInt)) env
    evalStatements env' fenv (outputs ++ exprOuts) rest

  MulAssignStmt name expr pos -> do
    current <-
      case Map.lookup name env of
        Just v -> Right v
        Nothing -> Left $ "Name error: undefined identifier " ++ name ++ " at " ++ showPos pos
    (rhs, exprOuts) <- evalExpr env fenv expr
    leftInt <- expectInt "*=" pos current
    rightInt <- expectInt "*=" pos rhs
    let env' = Map.insert name (IntValue (leftInt * rightInt)) env
    evalStatements env' fenv (outputs ++ exprOuts) rest

  DivAssignStmt name expr pos -> do
    current <-
      case Map.lookup name env of
        Just v -> Right v
        Nothing -> Left $ "Name error: undefined identifier " ++ name ++ " at " ++ showPos pos
    (rhs, exprOuts) <- evalExpr env fenv expr
    leftInt <- expectInt "/=" pos current
    rightInt <- expectInt "/=" pos rhs
    if rightInt == 0
      then Left $ "Value error: division by zero at " ++ showPos pos
      else do
        let env' = Map.insert name (IntValue (leftInt `div` rightInt)) env
        evalStatements env' fenv (outputs ++ exprOuts) rest

  ModAssignStmt name expr pos -> do
    current <-
      case Map.lookup name env of
        Just v -> Right v
        Nothing -> Left $ "Name error: undefined identifier " ++ name ++ " at " ++ showPos pos
    (rhs, exprOuts) <- evalExpr env fenv expr
    leftInt <- expectInt "%=" pos current
    rightInt <- expectInt "%=" pos rhs
    if rightInt == 0
      then Left $ "Value error: modulo by zero at " ++ showPos pos
      else do
        let env' = Map.insert name (IntValue (leftInt `mod` rightInt)) env
        evalStatements env' fenv (outputs ++ exprOuts) rest

  FloorDivAssignStmt name expr pos -> do
    current <-
      case Map.lookup name env of
        Just v -> Right v
        Nothing -> Left $ "Name error: undefined identifier " ++ name ++ " at " ++ showPos pos
    (rhs, exprOuts) <- evalExpr env fenv expr
    leftInt <- expectInt "//=" pos current
    rightInt <- expectInt "//=" pos rhs
    if rightInt == 0
      then Left $ "Value error: division by zero at " ++ showPos pos
      else do
        let env' = Map.insert name (IntValue (leftInt `div` rightInt)) env
        evalStatements env' fenv (outputs ++ exprOuts) rest

  PrintStmt expr _ ->
    case expr of
      StringExpr s _ -> evalStatements env fenv (outputs ++ [s]) rest
      _ -> do
        (val, exprOuts) <- evalExpr env fenv expr
        evalStatements env fenv (outputs ++ exprOuts ++ [valueToOutput val]) rest

  ReturnStmt expr pos -> do
    (val, exprOuts) <- evalExpr env fenv expr
    -- propagate return immediately, include any outputs produced evaluating the expression
    Right (env, fenv, outputs ++ exprOuts, Just (val, pos))

  BreakStmt pos -> Right (env, fenv, outputs, Just (BreakValue, pos))

  ContinueStmt pos -> Right (env, fenv, outputs, Just (ContinueValue, pos))

  GlobalStmt _ _ -> evalStatements env fenv outputs rest

  PassStmt _ -> evalStatements env fenv outputs rest

  IfStmt cond thenBranch maybeElse _ -> do
    (condVal, condOuts) <- evalExpr env fenv cond
    condNum <- expectTruthy "if condition" (exprPos cond) condVal
    if condNum /= 0
      then do
        (envThen, fenvThen, outputsThen, ret) <- evalStatements env fenv [] thenBranch
        case ret of
          Just (BreakValue, _) -> Right (envThen, fenvThen, outputs ++ condOuts ++ outputsThen, ret)
          Just (ContinueValue, _) -> Right (envThen, fenvThen, outputs ++ condOuts ++ outputsThen, ret)
          Just _ -> Right (envThen, fenvThen, outputs ++ condOuts ++ outputsThen, ret)
          Nothing -> evalStatements envThen fenvThen (outputs ++ condOuts ++ outputsThen) rest
      else case maybeElse of
        Just elseBranch -> do
          (envElse, fenvElse, outputsElse, ret) <- evalStatements env fenv [] elseBranch
          case ret of
            Just (BreakValue, _) -> Right (envElse, fenvElse, outputs ++ condOuts ++ outputsElse, ret)
            Just (ContinueValue, _) -> Right (envElse, fenvElse, outputs ++ condOuts ++ outputsElse, ret)
            Just _ -> Right (envElse, fenvElse, outputs ++ condOuts ++ outputsElse, ret)
            Nothing -> evalStatements envElse fenvElse (outputs ++ condOuts ++ outputsElse) rest
        Nothing -> evalStatements env fenv (outputs ++ condOuts) rest

  WhileStmt cond body whilePos -> loop env fenv outputs 0
    where
      loop env' fenv' outputs' iterations = do
        (condVal, condOuts) <- evalExpr env' fenv' cond
        condNum <- expectTruthy "while condition" (exprPos cond) condVal
        if condNum == 0
          then evalStatements env' fenv' (outputs' ++ condOuts) rest
          else
            if iterations >= maxLoopIterations
              then Left $ "Value error: iteration limit exceeded at " ++ showPos whilePos
              else do
                (envAfter, fenvAfter, outputsAfter, ret) <- evalStatements env' fenv' [] body
                case ret of
                  Just (BreakValue, _) -> evalStatements envAfter fenvAfter (outputs' ++ condOuts ++ outputsAfter) rest
                  Just (ContinueValue, _) -> loop envAfter fenvAfter (outputs' ++ condOuts ++ outputsAfter) (iterations + 1)
                  Just _ -> Right (envAfter, fenvAfter, outputs' ++ condOuts ++ outputsAfter, ret)
                  Nothing -> loop envAfter fenvAfter (outputs' ++ condOuts ++ outputsAfter) (iterations + 1)

  ForStmt name iterExpr body forPos -> do
    (iterVal, iterOuts) <- evalExpr env fenv iterExpr
    case iterVal of
      IntValue maxN ->
        let upper = max 0 maxN
         in loopRange env fenv 0 upper (outputs ++ iterOuts) 0
      ListValue vals ->
        loopList env fenv vals (outputs ++ iterOuts) 0
      DictValue pairs ->
        loopList env fenv (map fst pairs) (outputs ++ iterOuts) 0
      _ -> Left $ "Type error: for expects iterable (int range, list, or dict) at " ++ showPos (exprPos iterExpr)
    where
      loopRange env' fenv' idx upper outputs' iterations
        | idx >= upper = evalStatements env' fenv' outputs' rest
        | iterations >= maxLoopIterations = Left $ "Value error: iteration limit exceeded at " ++ showPos forPos
        | otherwise = do
            let envWithVar = Map.insert name (IntValue idx) env'
            (envAfter, fenvAfter, outputsAfter, ret) <- evalStatements envWithVar fenv' [] body
            case ret of
              Just (BreakValue, _) -> evalStatements envAfter fenvAfter (outputs' ++ outputsAfter) rest
              Just (ContinueValue, _) -> loopRange envAfter fenvAfter (idx + 1) upper (outputs' ++ outputsAfter) (iterations + 1)
              Just _ -> Right (envAfter, fenvAfter, outputs' ++ outputsAfter, ret)
              Nothing -> loopRange envAfter fenvAfter (idx + 1) upper (outputs' ++ outputsAfter) (iterations + 1)

      loopList env' fenv' [] outputs' _ = evalStatements env' fenv' outputs' rest
      loopList env' fenv' (value : remaining) outputs' iterations
        | iterations >= maxLoopIterations = Left $ "Value error: iteration limit exceeded at " ++ showPos forPos
        | otherwise = do
            let envWithVar = Map.insert name value env'
            (envAfter, fenvAfter, outputsAfter, ret) <- evalStatements envWithVar fenv' [] body
            case ret of
              Just (BreakValue, _) -> evalStatements envAfter fenvAfter (outputs' ++ outputsAfter) rest
              Just (ContinueValue, _) -> loopList envAfter fenvAfter remaining (outputs' ++ outputsAfter) (iterations + 1)
              Just _ -> Right (envAfter, fenvAfter, outputs' ++ outputsAfter, ret)
              Nothing -> loopList envAfter fenvAfter remaining (outputs' ++ outputsAfter) (iterations + 1)

  FunctionDefStmt name params body _ -> do
    let fenv' = Map.insert name (params, body) fenv
    evalStatements env fenv' outputs rest

-- evalExpr and evalArgs are local helpers (keeps a single top-level function in this file)
  where
    -- Expressions evaluate to (Value, [String]) where the second element is any outputs
    -- produced while evaluating the expression (e.g. from nested function calls).
    evalExpr :: Env -> FuncEnv -> Expr -> Either String (Value, [String])
    evalExpr _ _ (IntegerExpr n _) = Right (IntValue n, [])
    evalExpr _ _ (StringExpr s _) = Right (StringValue s, [])
    evalExpr _ _ (NoneExpr _) = Right (NoneValue, [])
    evalExpr env' fenv' (ListExpr exprs _) = do
      (vals, outs) <- evalArgs env' fenv' exprs
      Right (ListValue vals, outs)
    evalExpr env' fenv' (DictExpr entries _) = do
      (pairs, outs) <- evalDictEntries env' fenv' entries
      Right (DictValue pairs, outs)
    evalExpr env' _ (IdentifierExpr name pos) =
      case Map.lookup name env' of
        Just v -> Right (v, [])
        Nothing -> Left $ "Name error: undefined identifier " ++ name ++ " at " ++ showPos pos

    evalExpr env' fenv' (UnaryMinusExpr e pos) = do
      (v, outs) <- evalExpr env' fenv' e
      case v of
        IntValue n -> Right (IntValue (negate n), outs)
        _ -> Left $ "Type error: unary - expects int at " ++ showPos pos

    evalExpr env' fenv' (NotExpr e _) = do
      (v, outs) <- evalExpr env' fenv' e
      nv <- expectTruthy "not" (exprPos e) v
      Right (IntValue (if nv == 0 then 1 else 0), outs)

    evalExpr env' fenv' (BinaryExpr op l r pos) = case op of
      AddOperator -> do
        (lv, louts) <- evalExpr env' fenv' l
        (rv, routs) <- evalExpr env' fenv' r
        case (lv, rv) of
          (IntValue li, IntValue ri) -> Right (IntValue (li + ri), louts ++ routs)
          (StringValue ls, StringValue rs) -> Right (StringValue (ls ++ rs), louts ++ routs)
          _ -> Left $ "Type error: + expects int+int or string+string at " ++ showPos pos

      MultiplyOperator -> do
        (lv, louts) <- evalExpr env' fenv' l
        (rv, routs) <- evalExpr env' fenv' r
        li <- expectInt "*" pos lv
        ri <- expectInt "*" pos rv
        Right (IntValue (li * ri), louts ++ routs)

      DivideOperator -> do
        (lv, louts) <- evalExpr env' fenv' l
        (rv, routs) <- evalExpr env' fenv' r
        li <- expectInt "/" pos lv
        ri <- expectInt "/" pos rv
        if ri == 0
          then Left $ "Value error: division by zero at " ++ showPos pos
          else Right (IntValue (li `div` ri), louts ++ routs)

      ModuloOperator -> do
        (lv, louts) <- evalExpr env' fenv' l
        (rv, routs) <- evalExpr env' fenv' r
        li <- expectInt "%" pos lv
        ri <- expectInt "%" pos rv
        if ri == 0
          then Left $ "Value error: modulo by zero at " ++ showPos pos
          else Right (IntValue (li `mod` ri), louts ++ routs)

      EqOperator -> do
        (lv, louts) <- evalExpr env' fenv' l
        (rv, routs) <- evalExpr env' fenv' r
        Right (IntValue (if lv == rv then 1 else 0), louts ++ routs)

      NotEqOperator -> do
        (lv, louts) <- evalExpr env' fenv' l
        (rv, routs) <- evalExpr env' fenv' r
        Right (IntValue (if lv /= rv then 1 else 0), louts ++ routs)

      LtOperator -> do
        (lv, louts) <- evalExpr env' fenv' l
        (rv, routs) <- evalExpr env' fenv' r
        li <- expectInt "<" pos lv
        ri <- expectInt "<" pos rv
        Right (IntValue (if li < ri then 1 else 0), louts ++ routs)

      GtOperator -> do
        (lv, louts) <- evalExpr env' fenv' l
        (rv, routs) <- evalExpr env' fenv' r
        li <- expectInt ">" pos lv
        ri <- expectInt ">" pos rv
        Right (IntValue (if li > ri then 1 else 0), louts ++ routs)

      LteOperator -> do
        (lv, louts) <- evalExpr env' fenv' l
        (rv, routs) <- evalExpr env' fenv' r
        li <- expectInt "<=" pos lv
        ri <- expectInt "<=" pos rv
        Right (IntValue (if li <= ri then 1 else 0), louts ++ routs)

      GteOperator -> do
        (lv, louts) <- evalExpr env' fenv' l
        (rv, routs) <- evalExpr env' fenv' r
        li <- expectInt ">=" pos lv
        ri <- expectInt ">=" pos rv
        Right (IntValue (if li >= ri then 1 else 0), louts ++ routs)

      AndOperator -> do
        (lv, louts) <- evalExpr env' fenv' l
        li <- expectTruthy "and" pos lv
        if li == 0
          then Right (IntValue 0, louts)
          else do
            (rv, routs) <- evalExpr env' fenv' r
            ri <- expectTruthy "and" pos rv
            Right (IntValue (if ri /= 0 then 1 else 0), louts ++ routs)

      OrOperator -> do
        (lv, louts) <- evalExpr env' fenv' l
        li <- expectTruthy "or" pos lv
        if li /= 0
          then Right (IntValue 1, louts)
          else do
            (rv, routs) <- evalExpr env' fenv' r
            ri <- expectTruthy "or" pos rv
            Right (IntValue (if ri /= 0 then 1 else 0), louts ++ routs)

    evalExpr env' fenv' (CallExpr fname args pos) = case Map.lookup fname fenv' of
      _ | fname == "len" -> do
        (argVals, argOuts) <- evalArgs env' fenv' args
        case argVals of
          [StringValue s] -> Right (IntValue (length s), argOuts)
          [ListValue vals] -> Right (IntValue (length vals), argOuts)
          [_] -> Left $ "Type error: len expects string or list at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling len at " ++ showPos pos
      _ | fname == "bool" -> do
        (argVals, argOuts) <- evalArgs env' fenv' args
        case argVals of
          [IntValue n] -> Right (IntValue (if n == 0 then 0 else 1), argOuts)
          [NoneValue] -> Right (IntValue 0, argOuts)
          [StringValue s] -> Right (IntValue (if null s then 0 else 1), argOuts)
          [ListValue vals] -> Right (IntValue (if null vals then 0 else 1), argOuts)
          [DictValue pairs] -> Right (IntValue (if null pairs then 0 else 1), argOuts)
          _ -> Left $ "Argument count mismatch when calling bool at " ++ showPos pos
      _ | fname == "range" -> do
        (argVals, argOuts) <- evalArgs env' fenv' args
        case argVals of
          [IntValue n] -> Right (ListValue (map IntValue (rangeOne n)), argOuts)
          [IntValue start, IntValue stop] -> Right (ListValue (map IntValue (rangeWithStep start stop 1)), argOuts)
          [IntValue start, IntValue stop, IntValue step] ->
            if step == 0
              then Left $ "Value error: range step must not be zero at " ++ showPos pos
              else Right (ListValue (map IntValue (rangeWithStep start stop step)), argOuts)
          [_] -> Left $ "Type error: range expects int at " ++ showPos pos
          [_, _] -> Left $ "Type error: range expects int arguments at " ++ showPos pos
          [_, _, _] -> Left $ "Type error: range expects int arguments at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling range at " ++ showPos pos
      _ | fname == "append" -> do
        (argVals, argOuts) <- evalArgs env' fenv' args
        case argVals of
          [ListValue vals, value] -> Right (ListValue (vals ++ [value]), argOuts)
          [_, _] -> Left $ "Type error: append expects list as first argument at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling append at " ++ showPos pos
      _ | fname == "sort" -> do
        (argVals, argOuts) <- evalArgs env' fenv' args
        case argVals of
          [ListValue vals] ->
            case intValues vals of
              Just ns -> Right (ListValue (map IntValue (sort ns)), argOuts)
              Nothing -> Left $ "Type error: sort expects list of int at " ++ showPos pos
          [_] -> Left $ "Type error: sort expects list as first argument at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling sort at " ++ showPos pos
      _ | fname == "remove" -> do
        (argVals, argOuts) <- evalArgs env' fenv' args
        case argVals of
          [ListValue vals, target] ->
            case removeFirstValue vals target of
              Just newVals -> Right (ListValue newVals, argOuts)
              Nothing -> Left $ "Value error: remove value not found at " ++ showPos pos
          [_, _] -> Left $ "Type error: remove expects list as first argument at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling remove at " ++ showPos pos
      _ | fname == "insert" -> do
        (argVals, argOuts) <- evalArgs env' fenv' args
        case argVals of
          [ListValue vals, IntValue index, value] -> Right (ListValue (insertAtIndex vals index value), argOuts)
          [ListValue _, _, _] -> Left $ "Type error: insert expects int index at " ++ showPos pos
          [_, _, _] -> Left $ "Type error: insert expects list as first argument at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling insert at " ++ showPos pos
      _ | fname == "pop" -> do
        (argVals, argOuts) <- evalArgs env' fenv' args
        case argVals of
          [ListValue []] -> Left $ "Value error: pop from empty list at " ++ showPos pos
          [ListValue vals] -> Right (last vals, argOuts)
          [DictValue pairs, key] ->
            case lookupDictValue pairs key of
              Just value -> Right (value, argOuts)
              Nothing -> Left $ "Key not found in pop at " ++ showPos pos
          [DictValue pairs, key, defaultValue] ->
            case lookupDictValue pairs key of
              Just value -> Right (value, argOuts)
              Nothing -> Right (defaultValue, argOuts)
          [_] -> Left $ "Type error: pop expects list at " ++ showPos pos
          [ListValue _, _] -> Left $ "Argument count mismatch when calling pop at " ++ showPos pos
          [ListValue _, _, _] -> Left $ "Argument count mismatch when calling pop at " ++ showPos pos
          [_, _] -> Left $ "Type error: pop expects dict as first argument at " ++ showPos pos
          [_, _, _] -> Left $ "Type error: pop expects dict as first argument at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling pop at " ++ showPos pos
      _ | fname == "clear" -> do
        (argVals, argOuts) <- evalArgs env' fenv' args
        case argVals of
          [ListValue _] -> Right (ListValue [], argOuts)
          [DictValue _] -> Right (DictValue [], argOuts)
          [_] -> Left $ "Type error: clear expects list or dict at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling clear at " ++ showPos pos
      _ | fname == "keys" -> do
        (argVals, argOuts) <- evalArgs env' fenv' args
        case argVals of
          [DictValue pairs] -> Right (ListValue (map fst pairs), argOuts)
          [_] -> Left $ "Type error: keys expects dict at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling keys at " ++ showPos pos
      _ | fname == "get" -> do
        (argVals, argOuts) <- evalArgs env' fenv' args
        case argVals of
          [DictValue pairs, key] ->
            case lookupDictValue pairs key of
              Just value -> Right (value, argOuts)
              Nothing -> Left $ "Key not found in get at " ++ showPos pos
          [DictValue pairs, key, defaultValue] ->
            case lookupDictValue pairs key of
              Just value -> Right (value, argOuts)
              Nothing -> Right (defaultValue, argOuts)
          [_, _] -> Left $ "Type error: get expects dict as first argument at " ++ showPos pos
          [_, _, _] -> Left $ "Type error: get expects dict as first argument at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling get at " ++ showPos pos
      _ | fname == "update" -> do
        (argVals, argOuts) <- evalArgs env' fenv' args
        case argVals of
          [DictValue pairs, key, value] -> Right (DictValue (updateDictValue pairs key value), argOuts)
          [_, _, _] -> Left $ "Type error: update expects dict as first argument at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling update at " ++ showPos pos
      _ | fname == "setdefault" -> do
        (argVals, argOuts) <- evalArgs env' fenv' args
        case argVals of
          [DictValue pairs, key, defaultValue] -> Right (DictValue (setDefaultDictValue pairs key defaultValue), argOuts)
          [_, _, _] -> Left $ "Type error: setdefault expects dict as first argument at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling setdefault at " ++ showPos pos
      _ | fname == "values" -> do
        (argVals, argOuts) <- evalArgs env' fenv' args
        case argVals of
          [DictValue pairs] -> Right (ListValue (map snd pairs), argOuts)
          [_] -> Left $ "Type error: values expects dict at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling values at " ++ showPos pos
      _ | fname == "items" -> do
        (argVals, argOuts) <- evalArgs env' fenv' args
        case argVals of
          [DictValue pairs] -> Right (ListValue (map pairToList pairs), argOuts)
          [_] -> Left $ "Type error: items expects dict at " ++ showPos pos
          _ -> Left $ "Argument count mismatch when calling items at " ++ showPos pos
      Nothing -> Left $ "Name error: undefined function " ++ fname ++ " at " ++ showPos pos
      Just (params, body) -> do
        (argVals, argOuts) <- evalArgs env' fenv' args
        if length params /= length argVals
          then Left $ "Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos
          else do
            let localEnv = Map.union (Map.fromList (zip params argVals)) env'
            (_finalEnv, _finalFenv, bodyOuts, ret) <- evalStatements localEnv fenv' [] body
            let retVal = case ret of
                  Just (v, _) -> v
                  Nothing -> IntValue 0
            Right (retVal, argOuts ++ bodyOuts)

    -- Evaluate argument expressions left-to-right, collecting values and any outputs.
    evalArgs :: Env -> FuncEnv -> [Expr] -> Either String ([Value], [String])
    evalArgs env'' fenv'' = foldl go (Right ([], []))
      where
        go acc expr = do
          (vals, outs) <- acc
          (v, eouts) <- evalExpr env'' fenv'' expr
          Right (vals ++ [v], outs ++ eouts)

    evalDictEntries :: Env -> FuncEnv -> [(Expr, Expr)] -> Either String ([(Value, Value)], [String])
    evalDictEntries _ _ [] = Right ([], [])
    evalDictEntries env'' fenv'' ((keyExpr, valueExpr) : restEntries) = do
      (keyVal, keyOuts) <- evalExpr env'' fenv'' keyExpr
      (valueVal, valueOuts) <- evalExpr env'' fenv'' valueExpr
      (restVals, restOuts) <- evalDictEntries env'' fenv'' restEntries
      Right ((keyVal, valueVal) : restVals, keyOuts ++ valueOuts ++ restOuts)

    valueToOutput :: Value -> String
    valueToOutput (IntValue n) = show n
    valueToOutput (StringValue s) = s
    valueToOutput NoneValue = "None"
    valueToOutput (ListValue vals) = "[" ++ joinWithCommaSpace (map valueToOutput vals) ++ "]"
    valueToOutput (DictValue pairs) = "{" ++ joinWithCommaSpace (map pairToOutput pairs) ++ "}"
    valueToOutput BreakValue = "<break>"
    valueToOutput ContinueValue = "<continue>"

    pairToOutput (key, value) = valueToOutput key ++ ": " ++ valueToOutput value

    joinWithCommaSpace [] = ""
    joinWithCommaSpace [x] = x
    joinWithCommaSpace (x : xs) = x ++ ", " ++ joinWithCommaSpace xs

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

    maxLoopIterations :: Int
    maxLoopIterations = 10000
