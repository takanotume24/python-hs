module PythonHS.VM.CompileClassStmt (compileClassStmt) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AddOperator, AndOperator, EqOperator, GtOperator, LtOperator, OrOperator))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.VM.CollectClassMethods (collectClassMethods)
import PythonHS.VM.CompileClassStmtConfig (CompileClassStmtConfig (..))
import PythonHS.VM.CompileDefaultsResult (CompileDefaultsResult (..))
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.CompiledCallArg (CompiledCallArg (..))
import PythonHS.VM.CompiledDefaultCode (CompiledDefaultCode (..))
import PythonHS.VM.CompiledMethodDef (CompiledMethodDef (..))
import PythonHS.VM.DataclassCompilationResult (DataclassCompilationResult (..))
import PythonHS.VM.DataclassFlags (DataclassFlags (..))
import PythonHS.VM.FieldInfo (FieldInfo (..))
import PythonHS.VM.Instruction (Instruction (..))
import PythonHS.VM.MethodInfo (MethodInfo (..))
import PythonHS.VM.MethodKind (MethodKind (..))
import PythonHS.VM.MethodNamePair (MethodNamePair (..))

compileClassStmt :: CompileClassStmtConfig -> Either String CompileExprResult
compileClassStmt config = do
  methodsResult <- compileMethodsAt baseIndex methods
  dataclassResult <- compileDataclassMethods fields methodNames maybeDataclass
  let methodPairs = map (\m -> MethodNamePair {methodNamePairOriginal = methodInfoName m, methodNamePairMangled = className ++ "." ++ methodInfoName m, methodNamePairKind = methodInfoKind m}) methods
      allPairs = methodPairs ++ dataclassCompilationResultPairs dataclassResult
      classCode = compileExprResultCode methodsResult ++ dataclassCompilationResultCode dataclassResult ++ [DefineClass {defineClassName = className, defineClassBase = maybeBase, defineClassMethods = allPairs}]
  pure (CompileExprResult {compileExprResultCode = classCode, compileExprResultEndIndex = compileExprResultEndIndex methodsResult + dataclassCompilationResultCount dataclassResult + 1})
  where
    compileDefaultsFn = compileClassStmtCompileDefaults config
    compileStatements = compileClassStmtCompileStatements config
    compileExpr = compileClassStmtCompileExpr config
    baseIndex = compileClassStmtBaseIndex config
    className = compileClassStmtClassName config
    maybeBase = compileClassStmtMaybeBase config
    body = compileClassStmtBody config
    maybeDataclass = compileClassStmtMaybeDataclass config
    methods = collectClassMethods body
    fields = collectFields body
    methodNames = map methodInfoName methods

    compileMethodsAt idx [] = Right CompileExprResult {compileExprResultCode = [], compileExprResultEndIndex = idx}
    compileMethodsAt idx (methodInfo : restMethodInfos) = do
      let mangledName = className ++ "." ++ methodInfoName methodInfo
      defaultsResult <- compileDefaultsFn compileExpr (methodInfoDefaults methodInfo)
      bodyResult <- compileStatements 0 True Nothing (methodInfoBody methodInfo)
      let functionCode =
            if methodInfoName methodInfo == "__init__"
              then compileExprResultCode bodyResult ++ [LoadName {loadNameName = "self", loadNamePos = methodInfoPos methodInfo}, ReturnTop]
              else compileExprResultCode bodyResult ++ [PushConst {pushConstValue = IntValue {intValue = 0}}, ReturnTop]
      let methodInstr = DefineFunction {defineFunctionName = mangledName, defineFunctionParams = methodInfoParams methodInfo, defineFunctionDefaultCodes = compileDefaultsResultDefaults defaultsResult, defineFunctionCode = functionCode}
      let methodResult = CompileExprResult {compileExprResultCode = [methodInstr], compileExprResultEndIndex = idx + 1}
      restResult <- compileMethodsAt (compileExprResultEndIndex methodResult) restMethodInfos
      pure CompileExprResult {compileExprResultCode = compileExprResultCode methodResult ++ compileExprResultCode restResult, compileExprResultEndIndex = compileExprResultEndIndex restResult}

    collectFields items =
      case items of
        [] -> []
        item : restItems ->
          case item of
            AnnAssignStmt {annAssignStmtName = fieldName, annAssignStmtValue = maybeDefault, annAssignStmtPos = pos} ->
              FieldInfo {fieldInfoName = fieldName, fieldInfoDefault = maybeDefault, fieldInfoPos = pos} : collectFields restItems
            _ -> collectFields restItems

    compileDataclassMethods fields' methodNames' maybeFlags =
      case maybeFlags of
        Nothing -> Right DataclassCompilationResult {dataclassCompilationResultCode = [], dataclassCompilationResultPairs = [], dataclassCompilationResultCount = 0}
        Just flags -> do
          let isFrozen = dataclassFlagsFrozen flags
              isOrder = dataclassFlagsOrder flags
          initDefaults <- compileInitDefaults fields'
          let fieldNames = map fieldInfoName fields'
              methodPos = fieldPos fields'
              defineInit = buildInitMethod methodPos fieldNames initDefaults isFrozen
              defineRepr = buildReprMethod methodPos fieldNames
          defineEq <- buildEqMethod methodPos fieldNames
          ltMethod <- if isOrder then fmap Just (buildOrderMethod LtOperator methodPos fieldNames) else Right Nothing
          gtMethod <- if isOrder then fmap Just (buildOrderMethod GtOperator methodPos fieldNames) else Right Nothing
          let generated =
                concat
                  [ includeIfMissing "__init__" methodNames' [defineInit],
                    includeIfMissing "__repr__" methodNames' [defineRepr],
                    includeIfMissing "__eq__" methodNames' [defineEq],
                    includeIfMissing "__lt__" methodNames' (maybe [] (\m -> [m]) ltMethod),
                    includeIfMissing "__gt__" methodNames' (maybe [] (\m -> [m]) gtMethod)
                  ]
              names = map compiledMethodDefName generated
              pairs = map (\name -> MethodNamePair {methodNamePairOriginal = name, methodNamePairMangled = className ++ "." ++ name, methodNamePairKind = InstanceMethod}) names
              code = map (\m -> DefineFunction {defineFunctionName = className ++ "." ++ compiledMethodDefName m, defineFunctionParams = compiledMethodDefParams m, defineFunctionDefaultCodes = compiledMethodDefDefaults m, defineFunctionCode = compiledMethodDefBody m}) generated
          pure DataclassCompilationResult {dataclassCompilationResultCode = code, dataclassCompilationResultPairs = pairs, dataclassCompilationResultCount = length code}
      where
        includeIfMissing name names' values =
          if elem name names' then [] else values

        compileInitDefaults fields'' =
          compileDefaultsForFields fields'' []
          where
            compileDefaultsForFields items acc =
              case items of
                [] -> Right (reverse acc)
                FieldInfo {fieldInfoName = fieldName, fieldInfoDefault = maybeDefault} : rest ->
                  case maybeDefault of
                    Nothing -> compileDefaultsForFields rest acc
                    Just (CallExpr {callExprName = "field", callExprArgs = [KeywordArgExpr {keywordArgExprName = "default_factory", keywordArgExprValue = IdentifierExpr {identifierExprName = "list"}}]}) ->
                      compileDefaultsForFields rest (CompiledDefaultCode {compiledDefaultCodeParam = fieldName, compiledDefaultCodeInstructions = [BuildList {buildListCount = 0}, ReturnTop]} : acc)
                    Just defaultExpr -> do
                      defaultCode <- fmap compileExprResultCode (compileExpr 0 defaultExpr)
                      compileDefaultsForFields rest (CompiledDefaultCode {compiledDefaultCodeParam = fieldName, compiledDefaultCodeInstructions = defaultCode ++ [ReturnTop]} : acc)

        buildInitMethod pos fieldNames initDefaults isFrozen =
          let initBody =
                concatMap (\fieldName -> [LoadName {loadNameName = fieldName, loadNamePos = pos}, StoreName {storeNameName = "self." ++ fieldName}]) fieldNames
                  ++ frozenMarker isFrozen
                  ++ [LoadName {loadNameName = "self", loadNamePos = pos}, ReturnTop]
           in CompiledMethodDef {compiledMethodDefName = "__init__", compiledMethodDefParams = "self" : fieldNames, compiledMethodDefDefaults = initDefaults, compiledMethodDefBody = initBody}
          where
            frozenMarker frozen =
              if frozen
                then [PushConst {pushConstValue = IntValue {intValue = 1}}, StoreName {storeNameName = "self.__python_hs_frozen__"}]
                else []

        buildReprMethod pos fieldNames =
          let start = [PushConst {pushConstValue = StringValue {stringValue = className ++ "("}}]
              fieldParts = buildFieldParts fieldNames True
              endPart = [PushConst {pushConstValue = StringValue {stringValue = ")"}}, ApplyBinary {applyBinaryOp = AddOperator, applyBinaryPos = pos}, ReturnTop]
           in CompiledMethodDef {compiledMethodDefName = "__repr__", compiledMethodDefParams = ["self"], compiledMethodDefDefaults = [], compiledMethodDefBody = start ++ fieldParts ++ endPart}
          where
            buildFieldParts names isFirst =
              case names of
                [] -> []
                fieldName : rest ->
                  let prefix =
                        if isFirst
                          then fieldName ++ "="
                          else ", " ++ fieldName ++ "="
                      reprCall = CallFunction {callFunctionName = "__python_hs_repl_repr__", callFunctionArgs = [CompiledCallArg {compiledCallArgCode = [LoadName {loadNameName = "self." ++ fieldName, loadNamePos = pos}], compiledCallArgKeyword = Nothing, compiledCallArgPos = pos}], callFunctionPos = pos}
                   in [PushConst {pushConstValue = StringValue {stringValue = prefix}}, ApplyBinary {applyBinaryOp = AddOperator, applyBinaryPos = pos}, reprCall, ApplyBinary {applyBinaryOp = AddOperator, applyBinaryPos = pos}] ++ buildFieldParts rest False

        buildEqMethod pos fieldNames = do
          eqExpr <- buildEqExpr pos fieldNames
          eqCode <- fmap compileExprResultCode (compileExpr 0 eqExpr)
          pure CompiledMethodDef {compiledMethodDefName = "__eq__", compiledMethodDefParams = ["self", "other"], compiledMethodDefDefaults = [], compiledMethodDefBody = eqCode ++ [ReturnTop]}

        buildOrderMethod op pos fieldNames = do
          orderExpr <- buildOrderExpr op pos fieldNames
          orderCode <- fmap compileExprResultCode (compileExpr 0 orderExpr)
          pure CompiledMethodDef {compiledMethodDefName = methodName op, compiledMethodDefParams = ["self", "other"], compiledMethodDefDefaults = [], compiledMethodDefBody = orderCode ++ [ReturnTop]}
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
            FieldInfo {fieldInfoPos = pos} : _ -> pos
            [] -> Position {line = 0, column = 0}
