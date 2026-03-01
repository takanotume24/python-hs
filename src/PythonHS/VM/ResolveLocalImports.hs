module PythonHS.VM.ResolveLocalImports (resolveLocalImports) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import PythonHS.AST.Program (Program (Program))
import PythonHS.AST.Stmt
  ( Stmt
      ( AssignStmt,
        FromImportStmt,
        ImportStmt,
        FunctionDefDefaultsStmt,
        FunctionDefStmt
      )
  )
import PythonHS.Lexer.ScanTokens (scanTokens)
import PythonHS.Parser.ParseProgram (parseProgram)
import PythonHS.VM.TransformImportAliases (transformImportAliases)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

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
            FromImportStmt modulePath importedNames pos ->
              if modulePath == ["math"]
                then do
                  restResult <- resolveStmts cache visiting included moduleAlias callAlias identAlias rest
                  case restResult of
                    Left err -> pure (Left err)
                    Right (restStmts, cacheAfterRest, includedAfterRest) ->
                      pure (Right (FromImportStmt modulePath importedNames pos : restStmts, cacheAfterRest, includedAfterRest))
                else do
                  loadResult <- loadModule cache visiting included modulePath
                  case loadResult of
                    Left err -> pure (Left err)
                    Right (moduleStmts, exportMap, updatedCache, updatedIncluded) ->
                      let aliasPairs = fmap (\(memberName, maybeAlias) -> (maybe memberName id maybeAlias, Map.findWithDefault memberName memberName exportMap)) importedNames
                          updatedCallAlias = foldl (\m (k, v) -> Map.insert k v m) callAlias aliasPairs
                          updatedIdentAlias = foldl (\m (k, v) -> Map.insert k v m) identAlias aliasPairs
                       in do
                            restResult <- resolveStmts updatedCache visiting updatedIncluded moduleAlias updatedCallAlias updatedIdentAlias rest
                            case restResult of
                              Left err -> pure (Left err)
                              Right (restStmts, cacheAfterRest, includedAfterRest) ->
                                pure (Right (moduleStmts ++ restStmts, cacheAfterRest, includedAfterRest))
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
                      updatedModuleAlias = Map.insert aliasName modulePrefix moduleAlias
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

    collectExports modulePath stmts =
      foldl
        (\acc stmt ->
           case stmt of
             AssignStmt name _ _ -> Map.insert name (moduleMemberName name) acc
             FunctionDefStmt name _ _ _ -> Map.insert name (moduleMemberName name) acc
             FunctionDefDefaultsStmt name _ _ _ _ -> Map.insert name (moduleMemberName name) acc
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

    findModuleFile modulePath paths =
      let rel = foldl1Join "/" modulePath ++ ".py"
       in search rel paths
      where
        search relPath candidates =
          case candidates of
            [] -> pure (Left ("Import error: module not found " ++ foldl1Join "." modulePath))
            candidate : rest -> do
              let fullPath = candidate </> relPath
              exists <- doesFileExist fullPath
              if exists
                then pure (Right fullPath)
                else search relPath rest
