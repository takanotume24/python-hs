module PythonHS.VM.CompileFromStdlibImport (compileFromStdlibImport) where

import PythonHS.AST.FromImportEntry (FromImportEntry (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.CompileFromStdlibImportConfig (CompileFromStdlibImportConfig (..))
import PythonHS.VM.CompiledCallArg (CompiledCallArg (..))
import PythonHS.VM.Instruction (Instruction (..))
import PythonHS.VM.StdlibModuleValue (stdlibModuleValue)

-- | Compiles @from os/json/pathlib import ...@ into module-value bindings and
-- thin wrapper functions. Returns Nothing for modules this helper does not own.
compileFromStdlibImport :: CompileFromStdlibImportConfig -> Maybe (Either String CompileExprResult)
compileFromStdlibImport config =
  case modulePath of
    ["os"] -> Just (compileItems "os")
    ["json"] -> Just (compileItems "json")
    ["pathlib"] -> Just (compileItems "pathlib")
    ["sys"] -> Just (compileItems "sys")
    ["time"] -> Just (compileItems "time")
    ["random"] -> Just (compileItems "random")
    _ -> Nothing
  where
    baseIndex = compileFromStdlibImportBaseIndex config
    modulePath = compileFromStdlibImportModulePath config
    importedNames = compileFromStdlibImportImportedNames config
    pos = compileFromStdlibImportPos config

    compileItems moduleName = do
      let moduleAlias = moduleAliasFor moduleName
          setupCode =
            [ PushConst {pushConstValue = stdlibModuleValue moduleName},
              StoreName {storeNameName = moduleAlias}
            ]
      itemsResult <- compileEntries moduleAlias (baseIndex + length setupCode) importedNames
      pure
        ( CompileExprResult
            { compileExprResultCode = setupCode ++ compileExprResultCode itemsResult,
              compileExprResultEndIndex = compileExprResultEndIndex itemsResult
            }
        )

    compileEntries _ currentIndex [] =
      Right (CompileExprResult {compileExprResultCode = [], compileExprResultEndIndex = currentIndex})
    compileEntries moduleAlias currentIndex (entry : rest) = do
      firstResult <- compileEntry moduleAlias currentIndex entry
      restResult <- compileEntries moduleAlias (compileExprResultEndIndex firstResult) rest
      pure
        ( CompileExprResult
            { compileExprResultCode = compileExprResultCode firstResult ++ compileExprResultCode restResult,
              compileExprResultEndIndex = compileExprResultEndIndex restResult
            }
        )

    compileEntry moduleAlias currentIndex entry =
      case (moduleAlias, fromImportEntryName entry) of
        ("__python_hs_import_os", "path") ->
          Right
            ( CompileExprResult
                { compileExprResultCode = [PushConst {pushConstValue = ModuleValue {moduleValueName = "os.path", moduleValueAttrs = []}}, StoreName {storeNameName = targetName}],
                  compileExprResultEndIndex = currentIndex + 2
                }
            )
        (aliasName, memberName)
          | aliasName == "__python_hs_import_sys" && isSysMember memberName ->
              Right
                ( CompileExprResult
                    { compileExprResultCode =
                        [ LoadName {loadNameName = aliasName ++ "." ++ memberName, loadNamePos = pos},
                          StoreName {storeNameName = targetName}
                        ],
                      compileExprResultEndIndex = currentIndex + 2
                    }
                )
        (_, memberName) ->
          case memberArity moduleAlias memberName of
            Just arity -> Right (wrapperResult moduleAlias currentIndex entry memberName arity)
            Nothing -> Left ("Import error: unsupported module member " ++ memberName ++ " at " ++ showPos pos)
      where
        targetName =
          case fromImportEntryAlias entry of
            Just aliasName -> aliasName
            Nothing -> fromImportEntryName entry

    wrapperResult moduleAlias currentIndex entry memberName arity =
      let paramNames = ["__python_hs_import_arg" ++ show index | index <- [1 .. arity]]
          internalName = "__python_hs_stdlib_" ++ moduleAlias ++ "_" ++ memberName
          internalCallName = "__python_hs_stdlib_" ++ memberName
          moduleArg =
            CompiledCallArg
              { compiledCallArgCode = [LoadName {loadNameName = moduleAlias, loadNamePos = pos}],
                compiledCallArgKeyword = Nothing,
                compiledCallArgPos = pos
              }
          paramArgs = fmap toParamArg paramNames
          body = [CallFunction {callFunctionName = internalCallName, callFunctionArgs = moduleArg : paramArgs, callFunctionPos = pos}, ReturnTop]
          code =
            [ DefineFunction
                { defineFunctionName = internalName,
                  defineFunctionParams = paramNames,
                  defineFunctionDefaultCodes = [],
                  defineFunctionCode = body
                },
              PushConst {pushConstValue = FunctionRefValue {functionRefValueName = internalName, functionRefValueBindings = []}},
              StoreName {storeNameName = targetName}
            ]
       in CompileExprResult
            { compileExprResultCode = code,
              compileExprResultEndIndex = currentIndex + length code
            }
      where
        targetName =
          case fromImportEntryAlias entry of
            Just aliasName -> aliasName
            Nothing -> fromImportEntryName entry

        toParamArg paramName =
          CompiledCallArg
            { compiledCallArgCode = [LoadName {loadNameName = paramName, loadNamePos = pos}],
              compiledCallArgKeyword = Nothing,
              compiledCallArgPos = pos
            }

    memberArity :: String -> String -> Maybe Int
    memberArity moduleAlias memberName =
      case (moduleAlias, memberName) of
        ("__python_hs_import_os", "getcwd") -> Just 0
        ("__python_hs_import_json", "dumps") -> Just 1
        ("__python_hs_import_json", "loads") -> Just 1
        ("__python_hs_import_pathlib", "Path") -> Just 1
        ("__python_hs_import_time", "time") -> Just 0
        ("__python_hs_import_time", "sleep") -> Just 1
        ("__python_hs_import_random", "randint") -> Just 2
        ("__python_hs_import_random", "choice") -> Just 1
        _ -> Nothing

    isSysMember memberName =
      memberName `elem` ["argv", "path", "version", "platform"]

    moduleAliasFor moduleName = "__python_hs_import_" ++ moduleName
