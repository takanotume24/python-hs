module PythonHS.VM.ResolveLocalImports (resolveLocalImports) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import PythonHS.AST.Program (Program (Program))
import PythonHS.AST.Stmt
      ( Stmt
      ( AnnAssignStmt,
        AssignStmt,
        AssignUnpackStmt,
        DecoratedStmt,
        FromImportStmt,
        ImportStmt,
        FunctionDefDefaultsStmt,
        FunctionDefStmt
      )
  )
import PythonHS.Lexer.ScanTokens (scanTokens)
import PythonHS.Parser.ParseProgram (parseProgram)
import PythonHS.VM.FindModuleFile (findModuleFile)
import PythonHS.VM.TransformImportAliases (transformImportAliases)

resolveLocalImports :: [FilePath] -> Program -> IO (Either String Program)
resolveLocalImports searchPaths (Program rootStmts) = do
  result <- resolveStmts Map.empty Set.empty Set.empty Map.empty Map.empty Map.empty rootStmts
  pure (fmap (\(stmts, _, _) -> Program stmts) result)
  where
    resolveStmts cache visiting included moduleAlias callAlias identAlias stmts =
      case stmts of
        [] -> pure (Right ([], cache, included))
        stmt : rest ->
          case stmt of
            ImportStmt modules pos -> do
              importResult <- resolveImportEntries cache visiting included moduleAlias modules
              case importResult of
                Left err -> pure (Left err)
                Right (localPrefixed, updatedCache, updatedIncluded, updatedModuleAlias, keptMathEntries) -> do
                  restResult <- resolveStmts updatedCache visiting updatedIncluded updatedModuleAlias callAlias identAlias rest
                  case restResult of
                    Left err -> pure (Left err)
                    Right (restStmts, cacheAfterRest, includedAfterRest) ->
                      let mathStmt =
                            if null keptMathEntries
                              then []
                              else [ImportStmt keptMathEntries pos]
                       in pure (Right (localPrefixed ++ mathStmt ++ restStmts, cacheAfterRest, includedAfterRest))
            FromImportStmt relativeLevel modulePath importedNames pos
              | relativeLevel > 0 ->
                  pure (Left "Import error: relative import is not supported yet")
              | importedNames == [("*", Nothing)] ->
                  pure (Left "Import error: star import is not supported yet")
              | modulePath == ["math"] -> do
                  restResult <- resolveStmts cache visiting included moduleAlias callAlias identAlias rest
                  case restResult of
                    Left err -> pure (Left err)
                    Right (restStmts, cacheAfterRest, includedAfterRest) ->
                      pure (Right (FromImportStmt 0 modulePath importedNames pos : restStmts, cacheAfterRest, includedAfterRest))
              | modulePath == ["dataclasses"] -> do
                  restResult <- resolveStmts cache visiting included moduleAlias callAlias identAlias rest
                  case restResult of
                    Left err -> pure (Left err)
                    Right (restStmts, cacheAfterRest, includedAfterRest) ->
                      pure (Right (FromImportStmt 0 modulePath importedNames pos : restStmts, cacheAfterRest, includedAfterRest))
              | otherwise -> do
                  loadResult <- loadModule cache visiting included modulePath
                  case loadResult of
                    Left err -> pure (Left err)
                    Right (moduleStmts, exportMap, updatedCache, updatedIncluded) -> do
                      aliasResult <- resolveFromImports updatedCache visiting updatedIncluded modulePath moduleAlias callAlias identAlias [] importedNames exportMap
                      case aliasResult of
                        Left err -> pure (Left err)
                        Right (extraModuleStmts, cacheAfterAliases, includedAfterAliases, moduleAliasAfterAliases, callAliasAfterAliases, identAliasAfterAliases) -> do
                          restResult <- resolveStmts cacheAfterAliases visiting includedAfterAliases moduleAliasAfterAliases callAliasAfterAliases identAliasAfterAliases rest
                          case restResult of
                            Left err -> pure (Left err)
                            Right (restStmts, cacheAfterRest, includedAfterRest) ->
                              pure (Right (moduleStmts ++ extraModuleStmts ++ restStmts, cacheAfterRest, includedAfterRest))
            _ -> do
              let transformedStmt = transformImportAliases False moduleAlias callAlias identAlias stmt
              restResult <- resolveStmts cache visiting included moduleAlias callAlias identAlias rest
              case restResult of
                Left err -> pure (Left err)
                Right (restStmts, cacheAfterRest, includedAfterRest) -> pure (Right (transformedStmt : restStmts, cacheAfterRest, includedAfterRest))

    resolveImportEntries cache visiting included moduleAlias modules =
      case modules of
        [] -> pure (Right ([], cache, included, moduleAlias, []))
        (modulePath, maybeAlias) : rest ->
          if modulePath == ["math"]
            then do
              restResult <- resolveImportEntries cache visiting included moduleAlias rest
              pure (fmap (\(prefixed, nextCache, nextIncluded, nextModuleAlias, keptMath) -> (prefixed, nextCache, nextIncluded, nextModuleAlias, (modulePath, maybeAlias) : keptMath)) restResult)
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
                    Right (restPrefixed, cacheAfterRest, includedAfterRest, moduleAliasAfterRest, keptMath) ->
                      pure (Right (moduleStmts ++ restPrefixed, cacheAfterRest, includedAfterRest, moduleAliasAfterRest, keptMath))

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
                          resolvedModule <- resolveStmts cache (Set.insert moduleKey visiting) included Map.empty Map.empty Map.empty moduleStmts
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

    collectExports modulePath stmts =
      foldl
        (\acc stmt ->
           case stmt of
              AssignStmt name _ _ -> Map.insert name (moduleMemberName name) acc
              AssignUnpackStmt names _ _ -> foldl (\m name -> Map.insert name (moduleMemberName name) m) acc names
              AnnAssignStmt name _ (Just _) _ -> Map.insert name (moduleMemberName name) acc
              FunctionDefStmt name _ _ _ -> Map.insert name (moduleMemberName name) acc
              FunctionDefDefaultsStmt name _ _ _ _ -> Map.insert name (moduleMemberName name) acc
              DecoratedStmt _ innerStmt _ ->
                case innerStmt of
                  FunctionDefStmt name _ _ _ -> Map.insert name (moduleMemberName name) acc
                  FunctionDefDefaultsStmt name _ _ _ _ -> Map.insert name (moduleMemberName name) acc
                  _ -> acc
              _ -> acc
        )
        Map.empty
        stmts
      where
        moduleMemberName name = modulePrefixFor modulePath ++ name

    moduleKeyFor segments = foldl1Join "." segments

    modulePrefixFor segments = "__python_hs_local_" ++ foldl1Join "_" segments ++ "__"

    foldl1Join _ [] = ""
    foldl1Join _ [single] = single
    foldl1Join sep (x : xs) = x ++ sep ++ foldl1Join sep xs
