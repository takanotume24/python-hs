module PythonHS.VM.CompileImportStmt (compileImportStmt) where

import PythonHS.AST.Stmt (Stmt (FromImportStmt, ImportStmt))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (ModuleValue, StringValue))
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.Instruction (Instruction (CallFunction, DefineFunction, LoadName, PushConst, ReturnTop, StoreName))

compileImportStmt :: Int -> Stmt -> Either String CompileExprResult
compileImportStmt importBaseIndex stmt =
  case stmt of
    ImportStmt modules pos -> compileImportModules importBaseIndex modules pos
    FromImportStmt relativeLevel modulePath importedNames pos -> compileFromImport importBaseIndex relativeLevel modulePath importedNames pos
    _ -> Left "VM compile error: compileImportStmt called with non-import statement"
  where
    compileImportModules baseIndex modules pos =
      case modules of
        [] -> Right (CompileExprResult [] baseIndex)
        (modulePath, maybeAlias) : rest -> do
          firstResult <- compileSingleImport baseIndex modulePath maybeAlias pos
          restResult <- compileImportModules (compileExprResultEndIndex firstResult) rest pos
          pure (CompileExprResult (compileExprResultCode firstResult ++ compileExprResultCode restResult) (compileExprResultEndIndex restResult))

    compileSingleImport baseIndex modulePath maybeAlias pos =
      if null modulePath
        then Left ("Import error: unsupported module  at " ++ showPos pos)
        else
          case maybeAlias of
            Just aliasName ->
              let moduleName = joinModulePath modulePath
                  importCode = [PushConst (ModuleValue moduleName []), StoreName aliasName]
               in Right (CompileExprResult importCode (baseIndex + length importCode))
            Nothing ->
              case modulePath of
                [singleName] ->
                  let importCode = [PushConst (ModuleValue singleName []), StoreName singleName]
                   in Right (CompileExprResult importCode (baseIndex + length importCode))
                rootName : _ ->
                  let rootValue = buildRootModuleValue modulePath
                      importCode = [PushConst rootValue, StoreName rootName]
                   in Right (CompileExprResult importCode (baseIndex + length importCode))
                [] -> Left ("Import error: unsupported module  at " ++ showPos pos)

    compileFromImport baseIndex relativeLevel modulePath importedNames pos
      | relativeLevel > 0 =
          Left ("Import error: relative import is not supported in vm engine at " ++ showPos pos)
      | null importedNames =
          Left ("Import error: expected imported name at " ++ showPos pos)
      | modulePath == ["math"] = do
          let moduleAlias = "__python_hs_import_math"
              setupCode = [PushConst (StringValue "<module:math>"), StoreName moduleAlias]
          importedResult <- compileFromMathItems (baseIndex + 2) moduleAlias importedNames pos
          pure (CompileExprResult (setupCode ++ compileExprResultCode importedResult) (compileExprResultEndIndex importedResult))
      | modulePath == ["dataclasses"] =
          compileFromDataclassesItems baseIndex importedNames pos
      | otherwise =
          Left ("Import error: unsupported module " ++ joinModulePath modulePath ++ " at " ++ showPos pos)

    compileFromMathItems baseIndex moduleAlias importedNames pos =
      case importedNames of
        [] -> Right (CompileExprResult [] baseIndex)
        (name, maybeAlias) : rest -> do
          firstResult <- compileFromMathItem baseIndex moduleAlias name maybeAlias pos
          restResult <- compileFromMathItems (compileExprResultEndIndex firstResult) moduleAlias rest pos
          pure (CompileExprResult (compileExprResultCode firstResult ++ compileExprResultCode restResult) (compileExprResultEndIndex restResult))

    compileFromMathItem baseIndex moduleAlias name maybeAlias pos =
      let targetName =
            case maybeAlias of
              Just aliasName -> aliasName
              Nothing -> name
       in if name == "pi" || name == "e"
            then
              let callArgs = [([LoadName moduleAlias pos], Nothing, pos)]
               in Right (CompileExprResult [CallFunction name callArgs pos, StoreName targetName] (baseIndex + 2))
            else
              if isMathUnaryFunction name
                then
                  let wrapperParam = "__python_hs_import_arg"
                      callArgs = [([LoadName moduleAlias pos], Nothing, pos), ([LoadName wrapperParam pos], Nothing, pos)]
                      wrapperBody = [CallFunction name callArgs pos, ReturnTop]
                   in Right (CompileExprResult [DefineFunction targetName [wrapperParam] [] wrapperBody] (baseIndex + 1))
                else Left ("Import error: unsupported module member " ++ name ++ " at " ++ showPos pos)

    isMathUnaryFunction name =
      name == "sqrt"
        || name == "sin"
        || name == "cos"
        || name == "tan"
        || name == "log"
        || name == "exp"

    compileFromDataclassesItems baseIndex importedNames pos =
      case importedNames of
        [] -> Right (CompileExprResult [] baseIndex)
        (name, maybeAlias) : rest ->
          if name == "dataclass" || name == "field"
            then do
              let targetName =
                    case maybeAlias of
                      Just aliasName -> aliasName
                      Nothing -> name
                  firstCode = [PushConst (StringValue ("<dataclasses:" ++ name ++ ">")), StoreName targetName]
              restResult <- compileFromDataclassesItems (baseIndex + 2) rest pos
              pure (CompileExprResult (firstCode ++ compileExprResultCode restResult) (compileExprResultEndIndex restResult))
            else Left ("Import error: unsupported module member " ++ name ++ " at " ++ showPos pos)

    joinModulePath segments =
      case segments of
        [] -> ""
        [single] -> single
        segment : others -> segment ++ "." ++ joinModulePath others

    buildRootModuleValue segments =
      case segments of
        [] -> ModuleValue "" []
        root : rest ->
          ModuleValue root (buildAttrs root rest)

    buildAttrs _ [] = []
    buildAttrs prefix [leaf] =
      [(leaf, ModuleValue (prefix ++ "." ++ leaf) [])]
    buildAttrs prefix (x : xs) =
      let nextPrefix = prefix ++ "." ++ x
       in [(x, ModuleValue nextPrefix (buildAttrs nextPrefix xs))]
