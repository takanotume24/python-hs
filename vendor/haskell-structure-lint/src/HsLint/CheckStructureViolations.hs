module HsLint.CheckStructureViolations (checkStructureViolations) where

import Control.Monad (forM)
import Data.Char (isAlphaNum, isLower, isUpper, toUpper)
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf, nub, sort)
import Data.Maybe (listToMaybe, mapMaybe)
import HsLint.StructureConfig (StructureConfig (..))
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (dropExtension, makeRelative, splitDirectories, takeBaseName, takeExtension, (</>))

checkStructureViolations :: StructureConfig -> IO [String]
checkStructureViolations config = do
  hsFiles <- concat <$> forM (map (root </>) (structureRoots config)) collectHsFiles
  concat <$> forM (sort hsFiles) checkFile
  where
    root = structureRoot config

    collectHsFiles dir = do
      exists <- doesDirectoryExist dir
      if not exists
        then return []
        else do
          entries <- listDirectory dir
          children <- forM entries $ \entry -> do
            let path = dir </> entry
            isDir <- doesDirectoryExist path
            if isDir
              then collectHsFiles path
              else return [path | isHaskellFile path]
          return (concat children)

    isHaskellFile path = takeExtension path == ".hs"

    checkFile path = do
      content <- readFile path
      let relPath = makeRelative root path
          baseName = takeBaseName path
          isTestFile = hasAnyPrefix (structureTestDirPrefixes config) relPath
          isAppFile = hasAnyPrefix (structureAppDirPrefixes config) relPath
          isDiscoverDriver = Just relPath == structureDiscoverDriver config
          linesInFile = lines content
          topLevel = filter isSignificantTopLevel linesInFile
          moduleName = listToMaybe (mapMaybe parseModuleName topLevel)
          expectedModule = moduleNameFromPath relPath
          functionNames = nub (mapMaybe parseTopLevelFunction topLevel)
          typeNames = nub (mapMaybe parseTopLevelType topLevel)
          functionCount = length functionNames
          typeCount = length typeNames
          invalidFunctionNames = filter (not . isCamelCaseName) functionNames
          invalidTypeNames = filter (not . isPascalCaseName) typeNames
          expectedByFunction =
            case functionNames of
              [fn]
                | not isTestFile || fn /= "spec" -> [toPascalCase fn]
              _ -> []
          expectedByType =
            case typeNames of
              [ty] -> [ty]
              _ -> []
          expectedNames = nub (expectedByFunction ++ expectedByType)
          acceptsBaseNameModule = hasAnyPrefix (structureBasenameModulePrefixes config) relPath
          moduleViolations =
            if isDiscoverDriver
              then []
              else case moduleName of
                Nothing -> [relPath ++ ": missing module declaration"]
                Just actual
                  | isAppFile && acceptsBaseNameModule && actual == baseName -> []
                  | actual /= expectedModule -> [relPath ++ ": module name mismatch (expected " ++ expectedModule ++ ", got " ++ actual ++ ")"]
                  | otherwise -> []
          testNamingViolations =
            [relPath ++ ": test file name must end with " ++ structureTestSuffix config | isTestFile && not (structureTestSuffix config `isSuffixOf` baseName)]
          functionViolations =
            [relPath ++ ": top-level function declarations=" ++ show functionCount | functionCount > 1]
          functionNameViolations =
            map (\name -> relPath ++ ": function name is not camelCase: " ++ name) invalidFunctionNames
          typeViolations =
            [relPath ++ ": type/data/newtype/class declarations=" ++ show typeCount | typeCount > 1]
          typeNameViolations =
            map (\name -> relPath ++ ": type/class name is not PascalCase: " ++ name) invalidTypeNames
          fileNameViolations =
            [ relPath ++ ": file name mismatch (expected one of " ++ intercalate ", " expectedNames ++ ", got " ++ baseName ++ ")"
            | not (null expectedNames || baseName `elem` expectedNames || isTestFile)
            ]
          lineCountViolations =
            [relPath ++ ": file exceeds " ++ show (structureMaxLines config) ++ " lines (" ++ show (length linesInFile) ++ ")" | length linesInFile > structureMaxLines config, relPath `notElem` structureLineExemptions config]
      return (moduleViolations ++ testNamingViolations ++ functionViolations ++ functionNameViolations ++ typeViolations ++ typeNameViolations ++ fileNameViolations ++ lineCountViolations)

    hasAnyPrefix prefixes path = any (`isPrefixOf` path) prefixes

    isSignificantTopLevel line =
      not (null trimmed)
        && not (startsWith "--" trimmed)
        && not (startsWith "{-#" trimmed)
        && not (startsWith "{-" trimmed)
        && not (startsWith "#" trimmed)
        && not (startsWith "import " trimmed)
        && not (startsWith "deriving" trimmed)
        && not (startsWith "instance " trimmed)
        && not (startsWith "infix" trimmed)
        && not (startsWith "type family " trimmed)
        && not (startsWith "data family " trimmed)
        && not (isIndented line)
      where
        trimmed = trimLeft line

    isIndented line = case line of
      (' ' : _) -> True
      ('\t' : _) -> True
      _ -> False

    parseModuleName line =
      if startsWith "module " trimmed
        then case words trimmed of
          (_ : name : _) -> Just (takeWhile (/= '(') name)
          _ -> Nothing
        else Nothing
      where
        trimmed = trimLeft line

    parseTopLevelFunction line =
      if startsWithLowerIdent candidate && all validIdentChar candidate
        then Just candidate
        else Nothing
      where
        trimmed = trimLeft line
        candidate = takeWhile validIdentChar (takeBeforeDecl trimmed)

    parseTopLevelType line =
      case words (trimLeft line) of
        (kw : name : _)
          | kw `elem` ["data", "newtype", "class", "type"]
              && startsWithUpperIdent name
              && all validIdentChar name ->
              Just name
        _ -> Nothing

    takeBeforeDecl txt
      | "::" `isInfixOf` txt = takeUntil "::" txt
      | "=" `isInfixOf` txt = takeUntil "=" txt
      | otherwise = ""

    moduleNameFromPath relPath =
      intercalate "." $ case splitDirectories (dropExtension relPath) of
        (first : rest)
          | first `elem` structureModuleRootPrefixes config -> rest
        components -> components

    toPascalCase name =
      case splitByUnderscore name of
        [] -> name
        chunks -> concatMap capitalize chunks

    splitByUnderscore txt =
      let parts =
            foldr
              ( \c acc ->
                  if c == '_'
                    then "" : acc
                    else case acc of
                      [] -> [[c]]
                      (p : ps) -> (c : p) : ps
              )
              []
              txt
       in filter (not . null) parts

    capitalize [] = []
    capitalize (x : xs) = toUpper x : xs

    validIdentChar c = isAlphaNum c || c == '_' || c == '\''

    isCamelCaseName txt = startsWithLowerIdent txt && all validIdentChar txt

    isPascalCaseName txt = startsWithUpperIdent txt && all validIdentChar txt

    startsWithLowerIdent txt = case txt of
      (x : _) -> isLower x
      _ -> False

    startsWithUpperIdent txt = case txt of
      (x : _) -> isUpper x
      _ -> False

    trimLeft = dropWhile (== ' ')

    startsWith prefix txt = take (length prefix) txt == prefix

    takeUntil needle = go
      where
        go [] = []
        go rest@(x : xs)
          | startsWith needle rest = []
          | otherwise = x : go xs
