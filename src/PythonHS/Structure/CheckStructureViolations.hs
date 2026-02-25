module PythonHS.Structure.CheckStructureViolations (checkStructureViolations) where

import Control.Monad (forM)
import Data.Char (isAlphaNum, isLower, isUpper, toUpper)
import Data.List (intercalate, isPrefixOf, isSuffixOf, nub, sort)
import Data.Maybe (listToMaybe, mapMaybe)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (dropExtension, makeRelative, splitDirectories, takeBaseName, takeExtension, (</>))

checkStructureViolations :: FilePath -> IO [String]
checkStructureViolations root = do
  hsFiles <- fmap concat $ forM [root </> "src", root </> "app", root </> "test"] collectHsFiles
  fmap concat $ forM (sort hsFiles) checkFile
  where
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
          isTestFile = "test/" `isPrefixOf` relPath
          isAppFile = "app/" `isPrefixOf` relPath
          isDiscoverDriver = relPath == "test/Spec.hs"
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
          moduleViolations =
            if isDiscoverDriver
              then []
              else case moduleName of
                Nothing -> [relPath ++ ": missing module declaration"]
                Just actual
                  | isAppFile && actual == baseName -> []
                  | actual /= expectedModule -> [relPath ++ ": module name mismatch (expected " ++ expectedModule ++ ", got " ++ actual ++ ")"]
                  | otherwise -> []
          testNamingViolations =
            [relPath ++ ": test file name must end with Spec" | isTestFile && not ("Spec" `isSuffixOf` baseName)]
          functionViolations =
            [relPath ++ ": top-level function declarations=" ++ show functionCount | functionCount > 1]
          functionNameViolations =
            map (\name -> relPath ++ ": function name is not camelCase: " ++ name) invalidFunctionNames
          typeViolations =
            [relPath ++ ": type/data/newtype/class declarations=" ++ show typeCount | typeCount > 1]
          typeNameViolations =
            map (\name -> relPath ++ ": type/class name is not PascalCase: " ++ name) invalidTypeNames
          fileNameViolations =
            if null expectedNames || baseName `elem` expectedNames || isTestFile
              then []
              else [relPath ++ ": file name mismatch (expected one of " ++ intercalate ", " expectedNames ++ ", got " ++ baseName ++ ")"]
          lineCountViolations =
            [relPath ++ ": file exceeds 200 lines (" ++ show (length linesInFile) ++ ")" | not isTestFile, length linesInFile > 200, relPath `notElem` legacyLineCountExemptions]
      return (moduleViolations ++ testNamingViolations ++ functionViolations ++ functionNameViolations ++ typeViolations ++ typeNameViolations ++ fileNameViolations ++ lineCountViolations)

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
              && all validIdentChar name -> Just name
        _ -> Nothing

    takeBeforeDecl txt
      | "::" `isInfix` txt = takeUntil "::" txt
      | "=" `isInfix` txt = takeUntil "=" txt
      | otherwise = ""

    moduleNameFromPath relPath =
      intercalate "." $ case splitDirectories (dropExtension relPath) of
        ("src" : xs) -> xs
        ("app" : xs) -> xs
        ("test" : xs) -> xs
        xs -> xs

    toPascalCase name =
      case splitByUnderscore name of
        [] -> name
        chunks -> concatMap capitalize chunks

    splitByUnderscore txt =
      let parts = foldr
            (\c acc ->
              if c == '_'
                then "" : acc
                else case acc of
                  [] -> [[c]]
                  (p : ps) -> (c : p) : ps)
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

    isInfix needle haystack = not (null needle) && any (startsWith needle) (tails haystack)

    tails [] = [[]]
    tails xs@(_ : rest) = xs : tails rest

    takeUntil needle txt =
      go txt
      where
        go [] = []
        go rest@(x : xs)
          | startsWith needle rest = []
          | otherwise = x : go xs

    legacyLineCountExemptions = ["src/PythonHS/Evaluator/EvalStatements.hs"]
