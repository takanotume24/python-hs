module PythonHS.VM.CompileImportStmt (compileImportStmt) where

import PythonHS.AST.Stmt (Stmt (FromImportStmt, ImportStmt))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (StringValue))
import PythonHS.VM.Instruction (Instruction (CallFunction, DefineFunction, LoadName, PushConst, ReturnTop, StoreName))

compileImportStmt :: Int -> Stmt -> Either String ([Instruction], Int)
compileImportStmt importBaseIndex stmt =
  case stmt of
    ImportStmt modules pos -> compileImportModules importBaseIndex modules pos
    FromImportStmt modulePath importedNames pos -> compileFromImport importBaseIndex modulePath importedNames pos
    _ -> Left "VM compile error: compileImportStmt called with non-import statement"
  where
    compileImportModules baseIndex modules pos =
      case modules of
        [] -> Right ([], baseIndex)
        (modulePath, maybeAlias) : rest -> do
          (firstCode, firstEnd) <- compileSingleImport baseIndex modulePath maybeAlias pos
          (restCode, restEnd) <- compileImportModules firstEnd rest pos
          pure (firstCode ++ restCode, restEnd)

    compileSingleImport baseIndex modulePath maybeAlias pos =
      if modulePath == ["math"]
        then
          let targetName =
                case maybeAlias of
                  Just aliasName -> aliasName
                  Nothing -> "math"
           in Right ([PushConst (StringValue "<module:math>"), StoreName targetName], baseIndex + 2)
        else Left ("Import error: unsupported module " ++ joinModulePath modulePath ++ " at " ++ showPos pos)

    compileFromImport baseIndex modulePath importedNames pos =
      if null importedNames
        then Left ("Import error: expected imported name at " ++ showPos pos)
        else
          if modulePath == ["math"]
            then do
              let moduleAlias = "__python_hs_import_math"
                  setupCode = [PushConst (StringValue "<module:math>"), StoreName moduleAlias]
              (importedCode, importedEnd) <- compileFromMathItems (baseIndex + 2) moduleAlias importedNames pos
              pure (setupCode ++ importedCode, importedEnd)
            else Left ("Import error: unsupported module " ++ joinModulePath modulePath ++ " at " ++ showPos pos)

    compileFromMathItems baseIndex moduleAlias importedNames pos =
      case importedNames of
        [] -> Right ([], baseIndex)
        (name, maybeAlias) : rest -> do
          (firstCode, firstEnd) <- compileFromMathItem baseIndex moduleAlias name maybeAlias pos
          (restCode, restEnd) <- compileFromMathItems firstEnd moduleAlias rest pos
          pure (firstCode ++ restCode, restEnd)

    compileFromMathItem baseIndex moduleAlias name maybeAlias pos =
      let targetName =
            case maybeAlias of
              Just aliasName -> aliasName
              Nothing -> name
       in if name == "pi" || name == "e"
            then
              let callArgs = [([LoadName moduleAlias pos], Nothing, pos)]
               in Right ([CallFunction name callArgs pos, StoreName targetName], baseIndex + 2)
            else
              if isMathUnaryFunction name
                then
                  let wrapperParam = "__python_hs_import_arg"
                      callArgs = [([LoadName moduleAlias pos], Nothing, pos), ([LoadName wrapperParam pos], Nothing, pos)]
                      wrapperBody = [CallFunction name callArgs pos, ReturnTop]
                   in Right ([DefineFunction targetName [wrapperParam] [] wrapperBody], baseIndex + 1)
                else Left ("Import error: unsupported module member " ++ name ++ " at " ++ showPos pos)

    isMathUnaryFunction name =
      name == "sqrt"
        || name == "sin"
        || name == "cos"
        || name == "tan"
        || name == "log"
        || name == "exp"

    joinModulePath segments =
      case segments of
        [] -> ""
        [single] -> single
        segment : others -> segment ++ "." ++ joinModulePath others
