module PythonHS.VM.CompileImportStmt (compileImportStmt) where

import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.CompileImportStmtConfig (CompileImportStmtConfig (..))
import PythonHS.VM.Instruction (Instruction (..))

compileImportStmt :: CompileImportStmtConfig -> Either String CompileExprResult
compileImportStmt config = case stmt of
  ImportStmt {importStmtItems = modules, importStmtPos = pos} -> compileImportModules baseIndex modules pos
  FromImportStmt {fromImportStmtLevel = relativeLevel, fromImportStmtModule = modulePath, fromImportStmtItems = importedNames, fromImportStmtPos = pos} ->
    compileFromImport baseIndex relativeLevel modulePath importedNames pos
  _ -> Left "VM compile error: compileImportStmt called with non-import statement"
  where
    baseIndex = compileImportStmtBaseIndex config
    stmt = compileImportStmtStmt config
    compileImportModules currentBaseIndex modules pos =
      case modules of
        [] -> Right (CompileExprResult {compileExprResultCode = [], compileExprResultEndIndex = currentBaseIndex})
        (modulePath, maybeAlias) : rest -> do
          firstResult <- compileSingleImport currentBaseIndex modulePath maybeAlias pos
          restResult <- compileImportModules (compileExprResultEndIndex firstResult) rest pos
          pure
            ( CompileExprResult
                { compileExprResultCode = compileExprResultCode firstResult ++ compileExprResultCode restResult,
                  compileExprResultEndIndex = compileExprResultEndIndex restResult
                }
            )

    compileSingleImport currentBaseIndex modulePath maybeAlias pos =
      if null modulePath
        then Left ("Import error: unsupported module  at " ++ showPos pos)
        else case maybeAlias of
          Just aliasName ->
            let moduleName = joinModulePath modulePath
                importCode = [PushConst {pushConstValue = ModuleValue {moduleValueName = moduleName, moduleValueAttrs = []}}, StoreName {storeNameName = aliasName}]
             in Right (CompileExprResult {compileExprResultCode = importCode, compileExprResultEndIndex = currentBaseIndex + length importCode})
          Nothing ->
            case modulePath of
              [singleName] ->
                let importCode = [PushConst {pushConstValue = ModuleValue {moduleValueName = singleName, moduleValueAttrs = []}}, StoreName {storeNameName = singleName}]
                 in Right (CompileExprResult {compileExprResultCode = importCode, compileExprResultEndIndex = currentBaseIndex + length importCode})
              rootName : _ ->
                let rootValue = buildRootModuleValue modulePath
                    importCode = [PushConst {pushConstValue = rootValue}, StoreName {storeNameName = rootName}]
                 in Right (CompileExprResult {compileExprResultCode = importCode, compileExprResultEndIndex = currentBaseIndex + length importCode})
              [] -> Left ("Import error: unsupported module  at " ++ showPos pos)

    compileFromImport currentBaseIndex relativeLevel modulePath importedNames pos
      | relativeLevel > 0 =
          Left ("Import error: relative import is not supported in vm engine at " ++ showPos pos)
      | null importedNames =
          Left ("Import error: expected imported name at " ++ showPos pos)
      | modulePath == ["math"] = do
          let moduleAlias = "__python_hs_import_math"
              setupCode = [PushConst {pushConstValue = StringValue {stringValue = "<module:math>"}}, StoreName {storeNameName = moduleAlias}]
          importedResult <- compileFromMathItems (currentBaseIndex + 2) moduleAlias importedNames pos
          pure
            ( CompileExprResult
                { compileExprResultCode = setupCode ++ compileExprResultCode importedResult,
                  compileExprResultEndIndex = compileExprResultEndIndex importedResult
                }
            )
      | modulePath == ["dataclasses"] =
          compileFromDataclassesItems currentBaseIndex importedNames pos
      | otherwise =
          Left ("Import error: unsupported module " ++ joinModulePath modulePath ++ " at " ++ showPos pos)

    compileFromMathItems currentBaseIndex moduleAlias importedNames pos =
      case importedNames of
        [] -> Right (CompileExprResult {compileExprResultCode = [], compileExprResultEndIndex = currentBaseIndex})
        (name, maybeAlias) : rest -> do
          firstResult <- compileFromMathItem currentBaseIndex moduleAlias name maybeAlias pos
          restResult <- compileFromMathItems (compileExprResultEndIndex firstResult) moduleAlias rest pos
          pure
            ( CompileExprResult
                { compileExprResultCode = compileExprResultCode firstResult ++ compileExprResultCode restResult,
                  compileExprResultEndIndex = compileExprResultEndIndex restResult
                }
            )

    compileFromMathItem currentBaseIndex moduleAlias name maybeAlias pos =
      let targetName =
            case maybeAlias of
              Just aliasName -> aliasName
              Nothing -> name
       in if name == "pi" || name == "e"
            then
              let callArgs = [([LoadName {loadNameName = moduleAlias, loadNamePos = pos}], Nothing, pos)]
               in Right
                    ( CompileExprResult
                        { compileExprResultCode = [CallFunction {callFunctionName = name, callFunctionArgs = callArgs, callFunctionPos = pos}, StoreName {storeNameName = targetName}],
                          compileExprResultEndIndex = currentBaseIndex + 2
                        }
                    )
            else
              if isMathUnaryFunction name
                then
                  let wrapperParam = "__python_hs_import_arg"
                      callArgs = [([LoadName {loadNameName = moduleAlias, loadNamePos = pos}], Nothing, pos), ([LoadName {loadNameName = wrapperParam, loadNamePos = pos}], Nothing, pos)]
                      wrapperBody = [CallFunction {callFunctionName = name, callFunctionArgs = callArgs, callFunctionPos = pos}, ReturnTop]
                   in Right
                        ( CompileExprResult
                            { compileExprResultCode = [DefineFunction {defineFunctionName = targetName, defineFunctionParams = [wrapperParam], defineFunctionDefaultCodes = [], defineFunctionCode = wrapperBody}],
                              compileExprResultEndIndex = currentBaseIndex + 1
                            }
                        )
                else Left ("Import error: unsupported module member " ++ name ++ " at " ++ showPos pos)

    isMathUnaryFunction name =
      name == "sqrt"
        || name == "sin"
        || name == "cos"
        || name == "tan"
        || name == "log"
        || name == "exp"

    compileFromDataclassesItems currentBaseIndex importedNames pos =
      case importedNames of
        [] -> Right (CompileExprResult {compileExprResultCode = [], compileExprResultEndIndex = currentBaseIndex})
        (name, maybeAlias) : rest ->
          if name == "dataclass" || name == "field"
            then do
              let targetName =
                    case maybeAlias of
                      Just aliasName -> aliasName
                      Nothing -> name
                  firstCode = [PushConst {pushConstValue = StringValue {stringValue = "<dataclasses:" ++ name ++ ">"}}, StoreName {storeNameName = targetName}]
              restResult <- compileFromDataclassesItems (currentBaseIndex + 2) rest pos
              pure
                ( CompileExprResult
                    { compileExprResultCode = firstCode ++ compileExprResultCode restResult,
                      compileExprResultEndIndex = compileExprResultEndIndex restResult
                    }
                )
            else Left ("Import error: unsupported module member " ++ name ++ " at " ++ showPos pos)

    joinModulePath segments =
      case segments of
        [] -> ""
        [single] -> single
        segment : others -> segment ++ "." ++ joinModulePath others

    buildRootModuleValue segments =
      case segments of
        [] -> ModuleValue {moduleValueName = "", moduleValueAttrs = []}
        root : rest ->
          ModuleValue {moduleValueName = root, moduleValueAttrs = buildAttrs root rest}

    buildAttrs _ [] = []
    buildAttrs prefix [leaf] =
      [(leaf, ModuleValue {moduleValueName = prefix ++ "." ++ leaf, moduleValueAttrs = []})]
    buildAttrs prefix (x : xs) =
      let nextPrefix = prefix ++ "." ++ x
       in [(x, ModuleValue {moduleValueName = nextPrefix, moduleValueAttrs = buildAttrs nextPrefix xs})]
