module PythonHS.VM.FindModuleFile (findModuleFile) where

import PythonHS.VM.FindModuleFileConfig (FindModuleFileConfig (..))
import System.Directory (doesFileExist)
import System.FilePath ((</>))

findModuleFile :: FindModuleFileConfig -> IO (Either String FilePath)
findModuleFile config =
  let relModule = foldl1Join "/" modulePath ++ ".py"
      relPackageInit = foldl1Join "/" modulePath </> "__init__.py"
   in search [relModule, relPackageInit] paths
  where
    modulePath = findModuleFileModulePath config
    paths = findModuleFilePaths config
    search relPaths candidates =
      case candidates of
        [] -> pure (Left ("Import error: module not found " ++ foldl1Join "." modulePath))
        candidate : rest -> do
          found <- findFirstExisting candidate relPaths
          case found of
            Just fullPath -> pure (Right fullPath)
            Nothing -> search relPaths rest

    findFirstExisting _ [] = pure Nothing
    findFirstExisting candidate (relPath : restRelPaths) = do
      let fullPath = candidate </> relPath
      exists <- doesFileExist fullPath
      if exists
        then pure (Just fullPath)
        else findFirstExisting candidate restRelPaths

    foldl1Join _ [] = ""
    foldl1Join _ [single] = single
    foldl1Join sep (x : xs) = x ++ sep ++ foldl1Join sep xs
