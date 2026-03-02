module PythonHS.VM.CompileClassStmt (compileClassStmt) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AddOperator, AndOperator, EqOperator, GtOperator, LtOperator, OrOperator))
import PythonHS.AST.Expr (Expr (BinaryExpr, CallExpr, IdentifierExpr, IntegerExpr, KeywordArgExpr), Expr)
import PythonHS.AST.Stmt (Stmt (AnnAssignStmt, FunctionDefDefaultsStmt, FunctionDefStmt), Stmt)
import PythonHS.Evaluator.Value (Value (IntValue, StringValue))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.VM.Instruction (Instruction (ApplyBinary, BuildList, CallFunction, DefineClass, DefineFunction, LoadName, PushConst, ReturnTop, StoreName), Instruction)

compileClassStmt ::
  ((Int -> Expr -> Either String ([Instruction], Int)) -> [(String, Expr)] -> Either String ([(String, [Instruction])], Int)) ->
  (Int -> Bool -> Maybe (Int, Int) -> [Stmt] -> Either String ([Instruction], Int)) ->
  (Int -> Expr -> Either String ([Instruction], Int)) ->
  Int ->
  String ->
  Maybe String ->
  [Stmt] ->
  Maybe (Bool, Bool) ->
  Either String ([Instruction], Int)
compileClassStmt compileDefaults compileStatements compileExprAt baseIndex className maybeBase body maybeDataclass =
  let methods = collectMethods body
      fields = collectFields body
      methodNames = map (\(n, _, _, _, _) -> n) methods
      compileMethodAt idx (methodName, params, defaults, methodBody, methodPos) = do
        let mangledName = className ++ "." ++ methodName
        (defaultCodes, _) <- compileDefaults compileExprAt defaults
        (bodyCode, _) <- compileStatements 0 True Nothing methodBody
        let functionCode =
              if methodName == "__init__"
                then bodyCode ++ [LoadName "self" methodPos, ReturnTop]
                else bodyCode ++ [PushConst (IntValue 0), ReturnTop]
        let methodInstr = DefineFunction mangledName params defaultCodes functionCode
        Right ([methodInstr], idx + 1, (methodName, mangledName))
      compileMethodsAt idx [] = Right ([], idx, [])
      compileMethodsAt idx (method : restMethods) = do
        (methodCode, afterMethod, methodPair) <- compileMethodAt idx method
        (restCode, afterRest, restPairs) <- compileMethodsAt afterMethod restMethods
        Right (methodCode ++ restCode, afterRest, methodPair : restPairs)
   in do
        (methodCode, afterMethods, methodPairs) <- compileMethodsAt baseIndex methods
        (dataclassCode, dataclassPairs, dataclassCount) <- compileDataclassMethods fields methodNames maybeDataclass
        let classCode = methodCode ++ dataclassCode ++ [DefineClass className maybeBase (methodPairs ++ dataclassPairs)]
        Right (classCode, afterMethods + dataclassCount + 1)
  where
    collectMethods items =
      case items of
        [] -> []
        item : restItems ->
          case item of
            FunctionDefStmt methodName params methodBody methodPos ->
              (methodName, params, [], methodBody, methodPos) : collectMethods restItems
            FunctionDefDefaultsStmt methodName params defaults methodBody methodPos ->
              (methodName, params, defaults, methodBody, methodPos) : collectMethods restItems
            _ -> collectMethods restItems

    collectFields items =
      case items of
        [] -> []
        item : restItems ->
          case item of
            AnnAssignStmt fieldName _ maybeDefault pos -> (fieldName, maybeDefault, pos) : collectFields restItems
            _ -> collectFields restItems

    compileDataclassMethods _ _ Nothing = Right ([], [], 0)
    compileDataclassMethods fields methodNames (Just (isFrozen, isOrder)) = do
      initDefaults <- compileInitDefaults fields
      let fieldNames = map (\(name, _, _) -> name) fields
          methodPos = fieldPos fields
          defineInit = buildInitMethod methodPos fieldNames initDefaults isFrozen
          defineRepr = buildReprMethod methodPos fieldNames
      defineEq <- buildEqMethod methodPos fieldNames
      ltMethod <- if isOrder then buildOrderMethod LtOperator methodPos fieldNames else Right Nothing
      gtMethod <- if isOrder then buildOrderMethod GtOperator methodPos fieldNames else Right Nothing
      let generated =
            concat
              [ includeIfMissing "__init__" methodNames [defineInit],
                includeIfMissing "__repr__" methodNames [defineRepr],
                includeIfMissing "__eq__" methodNames [defineEq],
                includeIfMissing "__lt__" methodNames (maybe [] (\m -> [m]) ltMethod),
                includeIfMissing "__gt__" methodNames (maybe [] (\m -> [m]) gtMethod)
              ]
          pairs = map (\(name, _, _, _) -> (name, className ++ "." ++ name)) generated
          code = map (\(name, params, defaults, bodyCode) -> DefineFunction (className ++ "." ++ name) params defaults bodyCode) generated
      Right (code, pairs, length code)
      where
        includeIfMissing name names values =
          if elem name names
            then []
            else values

    compileInitDefaults fields =
      compileDefaultsForFields fields []
      where
        compileDefaultsForFields items acc =
          case items of
            [] -> Right (reverse acc)
            (fieldName, maybeDefault, _) : rest ->
              case maybeDefault of
                Nothing -> compileDefaultsForFields rest acc
                Just (CallExpr "field" [KeywordArgExpr "default_factory" (IdentifierExpr "list" _) _] _) ->
                  compileDefaultsForFields rest ((fieldName, [BuildList 0, ReturnTop]) : acc)
                Just defaultExpr -> do
                  (defaultCode, _) <- compileExprAt 0 defaultExpr
                  compileDefaultsForFields rest ((fieldName, defaultCode ++ [ReturnTop]) : acc)

    buildInitMethod pos fieldNames initDefaults isFrozen =
      let initBody =
            concatMap (\fieldName -> [LoadName fieldName pos, StoreName ("self." ++ fieldName)]) fieldNames
              ++ frozenMarker isFrozen
              ++ [LoadName "self" pos, ReturnTop]
       in ("__init__", "self" : fieldNames, initDefaults, initBody)
      where
        frozenMarker frozen =
          if frozen
            then [PushConst (IntValue 1), StoreName "self.__python_hs_frozen__"]
            else []

    buildReprMethod pos fieldNames =
      let start = [PushConst (StringValue (className ++ "("))]
          fieldParts = buildFieldParts fieldNames True
          endPart = [PushConst (StringValue ")"), ApplyBinary AddOperator pos, ReturnTop]
       in ("__repr__", ["self"], [], start ++ fieldParts ++ endPart)
      where
        buildFieldParts names isFirst =
          case names of
            [] -> []
            fieldName : rest ->
              let prefix =
                    if isFirst
                      then fieldName ++ "="
                      else ", " ++ fieldName ++ "="
                  reprCall = CallFunction "__python_hs_repl_repr__" [([LoadName ("self." ++ fieldName) pos], Nothing, pos)] pos
               in [PushConst (StringValue prefix), ApplyBinary AddOperator pos, reprCall, ApplyBinary AddOperator pos]
                    ++ buildFieldParts rest False

    buildEqMethod pos fieldNames = do
      eqExpr <- buildEqExpr pos fieldNames
      (eqCode, _) <- compileExprAt 0 eqExpr
      Right ("__eq__", ["self", "other"], [], eqCode ++ [ReturnTop])

    buildOrderMethod op pos fieldNames = do
      orderExpr <- buildOrderExpr op pos fieldNames
      (orderCode, _) <- compileExprAt 0 orderExpr
      Right (Just (methodName op, ["self", "other"], [], orderCode ++ [ReturnTop]))
      where
        methodName LtOperator = "__lt__"
        methodName GtOperator = "__gt__"
        methodName _ = "__order__"

    buildEqExpr pos fieldNames =
      case fieldNames of
        [] -> Right (IntegerExpr 1 pos)
        _ ->
          let comparisons = map (\fieldName -> BinaryExpr EqOperator (IdentifierExpr ("self." ++ fieldName) pos) (IdentifierExpr ("other." ++ fieldName) pos) pos) fieldNames
           in Right (foldAnd pos comparisons)

    buildOrderExpr op pos fieldNames =
      case fieldNames of
        [] -> Right (IntegerExpr 0 pos)
        [fieldName] ->
          Right (BinaryExpr op (IdentifierExpr ("self." ++ fieldName) pos) (IdentifierExpr ("other." ++ fieldName) pos) pos)
        fieldName : rest ->
          let leftExpr = BinaryExpr op (IdentifierExpr ("self." ++ fieldName) pos) (IdentifierExpr ("other." ++ fieldName) pos) pos
              eqExpr = BinaryExpr EqOperator (IdentifierExpr ("self." ++ fieldName) pos) (IdentifierExpr ("other." ++ fieldName) pos) pos
           in do
                restExpr <- buildOrderExpr op pos rest
                Right (BinaryExpr OrOperator leftExpr (BinaryExpr AndOperator eqExpr restExpr pos) pos)

    foldAnd pos exprs =
      case exprs of
        [] -> IntegerExpr 1 pos
        [single] -> single
        firstExpr : restExprs -> BinaryExpr AndOperator firstExpr (foldAnd pos restExprs) pos

    fieldPos fields =
      case fields of
        (_, _, pos) : _ -> pos
        [] -> Position 0 0
