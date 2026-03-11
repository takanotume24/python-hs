module PythonHS.VM.ResolveLocalImports (resolveLocalImports) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (isPrefixOf)
import PythonHS.AST.Program (Program (Program))
import PythonHS.AST.Stmt
      ( Stmt
      ( FromImportStmt,
        ImportStmt
      )
  )
import PythonHS.Lexer.ScanTokens (scanTokens)
import PythonHS.VM.CollectExports (collectExports)
import PythonHS.VM.ModuleKeyFor (moduleKeyFor)
import PythonHS.VM.ModulePrefixFor (modulePrefixFor)
import PythonHS.VM.ResolveTargetModulePath (resolveTargetModulePath)
import PythonHS.VM.IsBuiltinImportModule (isBuiltinImportModule)
import PythonHS.Parser.ParseProgram (parseProgram)
import PythonHS.VM.FindModuleFile (findModuleFile)
import PythonHS.VM.TransformImportAliases (transformImportAliases)
import System.FilePath (takeFileName)

resolveLocalImports :: [FilePath] -> Program -> IO (Either String Program)
resolveLocalImports searchPaths (Program rootStmts) = do
  result <- resolveStmts Map.empty Set.empty Set.empty [] Map.empty Map.empty Map.empty rootStmts
  pure (fmap (\(stmts, _, _) -> Program stmts) result)
  where
    resolveStmts cache visiting included currentPackage moduleAlias callAlias identAlias stmts =
      case stmts of
        [] -> pure (Right ([], cache, included))
        stmt : rest ->
          case stmt of
            ImportStmt modules pos -> do
              importResult <- resolveImportEntries cache visiting included moduleAlias modules
              case importResult of
                Left err -> pure (Left err)
                Right (localPrefixed, updatedCache, updatedIncluded, updatedModuleAlias, keptBuiltinEntries) -> do
                  restResult <- resolveStmts updatedCache visiting updatedIncluded currentPackage updatedModuleAlias callAlias identAlias rest
                  case restResult of
                    Left err -> pure (Left err)
                    Right (restStmts, cacheAfterRest, includedAfterRest) ->
                      let builtinStmt =
                            if null keptBuiltinEntries
                              then []
                              else [ImportStmt keptBuiltinEntries pos]
                       in pure (Right (localPrefixed ++ builtinStmt ++ restStmts, cacheAfterRest, includedAfterRest))
            FromImportStmt relativeLevel modulePath importedNames pos -> do
              let targetModulePath = resolveTargetModulePath currentPackage relativeLevel modulePath
              case targetModulePath of
                Left err -> pure (Left err)
                Right resolvedModulePath ->
                  if importedNames == [("*", Nothing)]
                    then do
                      starResult <- resolveStarImport cache visiting included resolvedModulePath callAlias identAlias
                      case starResult of
                        Left err -> pure (Left err)
                        Right (moduleStmts, updatedCache, updatedIncluded, updatedCallAlias, updatedIdentAlias) -> do
                          restResult <- resolveStmts updatedCache visiting updatedIncluded currentPackage moduleAlias updatedCallAlias updatedIdentAlias rest
                          case restResult of
                            Left err -> pure (Left err)
                            Right (restStmts, cacheAfterRest, includedAfterRest) ->
                              pure (Right (moduleStmts ++ restStmts, cacheAfterRest, includedAfterRest))
                    else
                      if resolvedModulePath == ["math"] || resolvedModulePath == ["dataclasses"]
                        then do
                          restResult <- resolveStmts cache visiting included currentPackage moduleAlias callAlias identAlias rest
                          case restResult of
                            Left err -> pure (Left err)
                            Right (restStmts, cacheAfterRest, includedAfterRest) ->
                              pure (Right (FromImportStmt 0 resolvedModulePath importedNames pos : restStmts, cacheAfterRest, includedAfterRest))
                        else do
                          loadResult <- loadModule cache visiting included resolvedModulePath
                          case loadResult of
                            Left err -> pure (Left err)
                            Right (moduleStmts, exportMap, updatedCache, updatedIncluded) -> do
                              aliasResult <- resolveFromImports updatedCache visiting updatedIncluded resolvedModulePath moduleAlias callAlias identAlias [] importedNames exportMap
                              case aliasResult of
                                Left err -> pure (Left err)
                                Right (extraModuleStmts, cacheAfterAliases, includedAfterAliases, moduleAliasAfterAliases, callAliasAfterAliases, identAliasAfterAliases) -> do
                                  restResult <- resolveStmts cacheAfterAliases visiting includedAfterAliases currentPackage moduleAliasAfterAliases callAliasAfterAliases identAliasAfterAliases rest
                                  case restResult of
                                    Left err -> pure (Left err)
                                    Right (restStmts, cacheAfterRest, includedAfterRest) ->
                                      pure (Right (moduleStmts ++ extraModuleStmts ++ restStmts, cacheAfterRest, includedAfterRest))
            _ -> do
              let transformedStmt = transformImportAliases False moduleAlias callAlias identAlias stmt
              restResult <- resolveStmts cache visiting included currentPackage moduleAlias callAlias identAlias rest
              case restResult of
                Left err -> pure (Left err)
                Right (restStmts, cacheAfterRest, includedAfterRest) -> pure (Right (transformedStmt : restStmts, cacheAfterRest, includedAfterRest))

    resolveImportEntries cache visiting included moduleAlias modules =
      case modules of
        [] -> pure (Right ([], cache, included, moduleAlias, []))
        (modulePath, maybeAlias) : rest ->
          if isBuiltinImportModule modulePath
            then do
              restResult <- resolveImportEntries cache visiting included moduleAlias rest
              pure (fmap (\(prefixed, nextCache, nextIncluded, nextModuleAlias, keptEntries) -> (prefixed, nextCache, nextIncluded, nextModuleAlias, (modulePath, maybeAlias) : keptEntries)) restResult)
            else do
              loadResult <- loadModule cache visiting included modulePath
              case loadResult of
                Left err -> pure (Left err)
                Right (moduleStmts, _exportMap, updatedCache, updatedIncluded) -> do
                  let aliasName = maybe (last modulePath) id maybeAlias
                      modulePrefix = modulePrefixFor modulePath
                      baseAliasMap = Map.insert aliasName modulePrefix moduleAlias
                      updatedModuleAlias =
                        case maybeAlias of
                          Just _ -> baseAliasMap
                          Nothing ->
                            if length modulePath > 1
                              then Map.insert (moduleKeyFor modulePath) modulePrefix baseAliasMap
                              else baseAliasMap
                  restResult <- resolveImportEntries updatedCache visiting updatedIncluded updatedModuleAlias rest
                  case restResult of
                    Left err -> pure (Left err)
                    Right (restPrefixed, cacheAfterRest, includedAfterRest, moduleAliasAfterRest, keptEntries) ->
                      pure (Right (moduleStmts ++ restPrefixed, cacheAfterRest, includedAfterRest, moduleAliasAfterRest, (modulePath, maybeAlias) : keptEntries))

    loadModule cache visiting included modulePath = do
      let moduleKey = moduleKeyFor modulePath
      if Set.member moduleKey visiting
        then pure (Left ("Import error: circular import " ++ moduleKey))
        else
          case Map.lookup moduleKey cache of
            Just (cachedStmts, cachedExports) ->
              if Set.member moduleKey included
                then pure (Right ([], cachedExports, cache, included))
                else pure (Right (cachedStmts, cachedExports, cache, Set.insert moduleKey included))
            Nothing -> do
              modulePathResult <- findModuleFile modulePath searchPaths
              case modulePathResult of
                Left err -> pure (Left err)
                Right moduleFile -> do
                  source <- readFile moduleFile
                  case scanTokens source of
                    Left lexErr -> pure (Left (show lexErr))
                    Right tokens ->
                      case parseProgram tokens of
                        Left parseErr -> pure (Left (show parseErr))
                        Right (Program moduleStmts) -> do
                          let packagePath =
                                if takeFileName moduleFile == "__init__.py"
                                  then modulePath
                                  else dropLast modulePath
                          resolvedModule <- resolveStmts cache (Set.insert moduleKey visiting) included packagePath Map.empty Map.empty Map.empty moduleStmts
                          case resolvedModule of
                            Left err -> pure (Left err)
                            Right (moduleResolvedStmts, cacheAfterResolve, includedAfterResolve) -> do
                              let exportMap = collectExports modulePath moduleResolvedStmts
                                  renamedModuleStmts = fmap (transformImportAliases True Map.empty exportMap exportMap) moduleResolvedStmts
                                  updatedCache = Map.insert moduleKey (renamedModuleStmts, exportMap) cacheAfterResolve
                              if Set.member moduleKey includedAfterResolve
                                then pure (Right ([], exportMap, updatedCache, includedAfterResolve))
                                else pure (Right (renamedModuleStmts, exportMap, updatedCache, Set.insert moduleKey includedAfterResolve))

    resolveFromImports cache visiting included modulePath moduleAlias callAlias identAlias collected importedNames exportMap =
      case importedNames of
        [] -> pure (Right (reverse collected, cache, included, moduleAlias, callAlias, identAlias))
        (memberName, maybeAlias) : rest ->
          case Map.lookup memberName exportMap of
            Just mappedName ->
              let aliasName = maybe memberName id maybeAlias
               in resolveFromImports cache visiting included modulePath moduleAlias (Map.insert aliasName mappedName callAlias) (Map.insert aliasName mappedName identAlias) collected rest exportMap
            Nothing -> do
              let submodulePath = modulePath ++ [memberName]
              moduleFileResult <- findModuleFile submodulePath searchPaths
              case moduleFileResult of
                Left _ ->
                  pure (Left ("Import error: name not found " ++ memberName ++ " in " ++ moduleKeyFor modulePath))
                Right _ -> do
                  submoduleResult <- loadModule cache visiting included submodulePath
                  case submoduleResult of
                    Left err -> pure (Left err)
                    Right (submoduleStmts, _submoduleExports, updatedCache, updatedIncluded) ->
                      let aliasName = maybe memberName id maybeAlias
                          modulePrefix = modulePrefixFor submodulePath
                       in resolveFromImports updatedCache visiting updatedIncluded modulePath (Map.insert aliasName modulePrefix moduleAlias) callAlias identAlias (reverse submoduleStmts ++ collected) rest exportMap

    dropLast segments =
      case segments of
        [] -> []
        [_] -> []
        x : xs -> x : dropLast xs

    resolveStarImport cache visiting included modulePath callAlias identAlias = do
      loadResult <- loadModule cache visiting included modulePath
      case loadResult of
        Left err -> pure (Left err)
        Right (moduleStmts, exportMap, updatedCache, updatedIncluded) ->
          let visibleExports = Map.toList (Map.filterWithKey (\name _ -> not ("_" `isPrefixOf` name)) exportMap)
              updatedCallAlias = foldl (\m (k, v) -> Map.insert k v m) callAlias visibleExports
              updatedIdentAlias = foldl (\m (k, v) -> Map.insert k v m) identAlias visibleExports
           in pure (Right (moduleStmts, updatedCache, updatedIncluded, updatedCallAlias, updatedIdentAlias))
