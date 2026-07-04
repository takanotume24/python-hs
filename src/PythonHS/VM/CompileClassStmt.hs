module PythonHS.VM.CompileClassStmt (compileClassStmt) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AddOperator, AndOperator, EqOperator, GtOperator, LtOperator, OrOperator))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.VM.CompileClassStmtConfig (CompileClassStmtConfig (..))
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.Instruction (Instruction (..))

compileClassStmt :: CompileClassStmtConfig -> Either String CompileExprResult
compileClassStmt config = do
  (methodsResult, methodPairs) <- compileMethodsAt baseIndex methods
  (dataclassCode, dataclassPairs, dataclassCount) <- compileDataclassMethods fields methodNames maybeDataclass
  let classCode = compileExprResultCode methodsResult ++ dataclassCode ++ [DefineClass {defineClassName = className, defineClassBase = maybeBase, defineClassMethods = methodPairs ++ dataclassPairs}]
  pure (CompileExprResult {compileExprResultCode = classCode, compileExprResultEndIndex = compileExprResultEndIndex methodsResult + dataclassCount + 1})
  where
    compileDefaultsFn = compileClassStmtCompileDefaults config
    compileStatements = compileClassStmtCompileStatements config
    compileExpr = compileClassStmtCompileExpr config
    baseIndex = compileClassStmtBaseIndex config
    className = compileClassStmtClassName config
    maybeBase = compileClassStmtMaybeBase config
    body = compileClassStmtBody config
    maybeDataclass = compileClassStmtMaybeDataclass config
    methods = collectMethods body
    fields = collectFields body
    methodNames = map (\(n, _, _, _, _) -> n) methods
    compileMethodAt idx (methodName, params, defaults, methodBody, methodPos) = do
      let mangledName = className ++ "." ++ methodName
      (defaultCodes, _) <- compileDefaultsFn compileExpr defaults
      bodyResult <- compileStatements 0 True Nothing methodBody
      let functionCode =
            if methodName == "__init__"
              then compileExprResultCode bodyResult ++ [LoadName {loadNameName = "self", loadNamePos = methodPos}, ReturnTop]
              else compileExprResultCode bodyResult ++ [PushConst {pushConstValue = IntValue {intValue = 0}}, ReturnTop]
      let methodInstr = DefineFunction {defineFunctionName = mangledName, defineFunctionParams = params, defineFunctionDefaultCodes = defaultCodes, defineFunctionCode = functionCode}
      Right (CompileExprResult {compileExprResultCode = [methodInstr], compileExprResultEndIndex = idx + 1}, (methodName, mangledName))
    compileMethodsAt idx [] = Right (CompileExprResult {compileExprResultCode = [], compileExprResultEndIndex = idx}, [])
    compileMethodsAt idx (method : restMethods) = do
      (methodResult, methodPair) <- compileMethodAt idx method
      (restResult, restPairs) <- compileMethodsAt (compileExprResultEndIndex methodResult) restMethods
      pure (CompileExprResult {compileExprResultCode = compileExprResultCode methodResult ++ compileExprResultCode restResult, compileExprResultEndIndex = compileExprResultEndIndex restResult}, methodPair : restPairs)

    collectMethods items =
      case items of
        [] -> []
        item : restItems ->
          case item of
            FunctionDefStmt {functionDefStmtName = methodName, functionDefStmtParams = params, functionDefStmtBody = methodBody, functionDefStmtPos = methodPos} ->
              (methodName, params, [], methodBody, methodPos) : collectMethods restItems
            FunctionDefDefaultsStmt {functionDefDefaultsStmtName = methodName, functionDefDefaultsStmtParams = params, functionDefDefaultsStmtDefaults = defaults, functionDefDefaultsStmtBody = methodBody, functionDefDefaultsStmtPos = methodPos} ->
              (methodName, params, defaults, methodBody, methodPos) : collectMethods restItems
            _ -> collectMethods restItems

    collectFields items =
      case items of
        [] -> []
        item : restItems ->
          case item of
            AnnAssignStmt {annAssignStmtName = fieldName, annAssignStmtValue = maybeDefault, annAssignStmtPos = pos} -> (fieldName, maybeDefault, pos) : collectFields restItems
            _ -> collectFields restItems

    compileDataclassMethods _ _ Nothing = Right ([], [], 0)
    compileDataclassMethods fields' methodNames' (Just (isFrozen, isOrder)) = do
      initDefaults <- compileInitDefaults fields'
      let fieldNames = map (\(name, _, _) -> name) fields'
          methodPos = fieldPos fields'
          defineInit = buildInitMethod methodPos fieldNames initDefaults isFrozen
          defineRepr = buildReprMethod methodPos fieldNames
      defineEq <- buildEqMethod methodPos fieldNames
      ltMethod <- if isOrder then buildOrderMethod LtOperator methodPos fieldNames else Right Nothing
      gtMethod <- if isOrder then buildOrderMethod GtOperator methodPos fieldNames else Right Nothing
      let generated =
            concat
              [ includeIfMissing "__init__" methodNames' [defineInit],
                includeIfMissing "__repr__" methodNames' [defineRepr],
                includeIfMissing "__eq__" methodNames' [defineEq],
                includeIfMissing "__lt__" methodNames' (maybe [] (\m -> [m]) ltMethod),
                includeIfMissing "__gt__" methodNames' (maybe [] (\m -> [m]) gtMethod)
              ]
          pairs = map (\(name, _, _, _) -> (name, className ++ "." ++ name)) generated
          code = map (\(name, params, defaults, bodyCode) -> DefineFunction {defineFunctionName = className ++ "." ++ name, defineFunctionParams = params, defineFunctionDefaultCodes = defaults, defineFunctionCode = bodyCode}) generated
      Right (code, pairs, length code)
      where
        includeIfMissing name names values =
          if elem name names
            then []
            else values

        compileInitDefaults fields'' =
          compileDefaultsForFields fields'' []
          where
            compileDefaultsForFields items acc =
              case items of
                [] -> Right (reverse acc)
                (fieldName, maybeDefault, _) : rest ->
                  case maybeDefault of
                    Nothing -> compileDefaultsForFields rest acc
                    Just (CallExpr {callExprName = "field", callExprArgs = [KeywordArgExpr {keywordArgExprName = "default_factory", keywordArgExprValue = IdentifierExpr {identifierExprName = "list"}}]}) ->
                      compileDefaultsForFields rest ((fieldName, [BuildList {buildListCount = 0}, ReturnTop]) : acc)
                    Just defaultExpr -> do
                      defaultCode <- fmap compileExprResultCode (compileExpr 0 defaultExpr)
                      compileDefaultsForFields rest ((fieldName, defaultCode ++ [ReturnTop]) : acc)

        buildInitMethod pos fieldNames initDefaults isFrozen =
          let initBody =
                concatMap (\fieldName -> [LoadName {loadNameName = fieldName, loadNamePos = pos}, StoreName {storeNameName = "self." ++ fieldName}]) fieldNames
                  ++ frozenMarker isFrozen
                  ++ [LoadName {loadNameName = "self", loadNamePos = pos}, ReturnTop]
           in ("__init__", "self" : fieldNames, initDefaults, initBody)
          where
            frozenMarker frozen =
              if frozen
                then [PushConst {pushConstValue = IntValue {intValue = 1}}, StoreName {storeNameName = "self.__python_hs_frozen__"}]
                else []

        buildReprMethod pos fieldNames =
          let start = [PushConst {pushConstValue = StringValue {stringValue = className ++ "("}}]
              fieldParts = buildFieldParts fieldNames True
              endPart = [PushConst {pushConstValue = StringValue {stringValue = ")"}}, ApplyBinary {applyBinaryOp = AddOperator, applyBinaryPos = pos}, ReturnTop]
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
                      reprCall = CallFunction {callFunctionName = "__python_hs_repl_repr__", callFunctionArgs = [([LoadName {loadNameName = "self." ++ fieldName, loadNamePos = pos}], Nothing, pos)], callFunctionPos = pos}
                   in [PushConst {pushConstValue = StringValue {stringValue = prefix}}, ApplyBinary {applyBinaryOp = AddOperator, applyBinaryPos = pos}, reprCall, ApplyBinary {applyBinaryOp = AddOperator, applyBinaryPos = pos}]
                        ++ buildFieldParts rest False

        buildEqMethod pos fieldNames = do
          eqExpr <- buildEqExpr pos fieldNames
          eqCode <- fmap compileExprResultCode (compileExpr 0 eqExpr)
          Right ("__eq__", ["self", "other"], [], eqCode ++ [ReturnTop])

        buildOrderMethod op pos fieldNames = do
          orderExpr <- buildOrderExpr op pos fieldNames
          orderCode <- fmap compileExprResultCode (compileExpr 0 orderExpr)
          Right (Just (methodName op, ["self", "other"], [], orderCode ++ [ReturnTop]))
          where
            methodName LtOperator = "__lt__"
            methodName GtOperator = "__gt__"
            methodName _ = "__order__"

        buildEqExpr pos fieldNames =
          case fieldNames of
            [] -> Right (IntegerExpr {integerExprValue = 1, integerExprPos = pos})
            _ ->
              let comparisons = map (\fieldName -> BinaryExpr {binaryExprOp = EqOperator, binaryExprLeft = IdentifierExpr {identifierExprName = "self." ++ fieldName, identifierExprPos = pos}, binaryExprRight = IdentifierExpr {identifierExprName = "other." ++ fieldName, identifierExprPos = pos}, binaryExprPos = pos}) fieldNames
               in Right (foldAnd pos comparisons)

        buildOrderExpr op pos fieldNames =
          case fieldNames of
            [] -> Right (IntegerExpr {integerExprValue = 0, integerExprPos = pos})
            [fieldName] ->
              Right (BinaryExpr {binaryExprOp = op, binaryExprLeft = IdentifierExpr {identifierExprName = "self." ++ fieldName, identifierExprPos = pos}, binaryExprRight = IdentifierExpr {identifierExprName = "other." ++ fieldName, identifierExprPos = pos}, binaryExprPos = pos})
            fieldName : rest ->
              let leftExpr = BinaryExpr {binaryExprOp = op, binaryExprLeft = IdentifierExpr {identifierExprName = "self." ++ fieldName, identifierExprPos = pos}, binaryExprRight = IdentifierExpr {identifierExprName = "other." ++ fieldName, identifierExprPos = pos}, binaryExprPos = pos}
                  eqExpr = BinaryExpr {binaryExprOp = EqOperator, binaryExprLeft = IdentifierExpr {identifierExprName = "self." ++ fieldName, identifierExprPos = pos}, binaryExprRight = IdentifierExpr {identifierExprName = "other." ++ fieldName, identifierExprPos = pos}, binaryExprPos = pos}
               in do
                    restExpr <- buildOrderExpr op pos rest
                    Right (BinaryExpr {binaryExprOp = OrOperator, binaryExprLeft = leftExpr, binaryExprRight = BinaryExpr {binaryExprOp = AndOperator, binaryExprLeft = eqExpr, binaryExprRight = restExpr, binaryExprPos = pos}, binaryExprPos = pos})

        foldAnd pos exprs =
          case exprs of
            [] -> IntegerExpr {integerExprValue = 1, integerExprPos = pos}
            [single] -> single
            firstExpr : restExprs -> BinaryExpr {binaryExprOp = AndOperator, binaryExprLeft = firstExpr, binaryExprRight = foldAnd pos restExprs, binaryExprPos = pos}

        fieldPos fields'' =
          case fields'' of
            (_, _, pos) : _ -> pos
            [] -> Position {line = 0, column = 0}
