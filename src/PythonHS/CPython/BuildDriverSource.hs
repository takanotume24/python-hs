module PythonHS.CPython.BuildDriverSource (buildDriverSource) where

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import PythonHS.CPython.CaseId (CaseId (..))

-- | Build a small driver program that runs one CPython test method.
--
-- The CPython test files contain methods that use language features the
-- interpreter does not support yet. Only the module-level imports, the class
-- header, and the requested method are copied into the driver so that the
-- allowlisted method can be executed without parsing unrelated methods.
buildDriverSource :: String -> CaseId -> Either String String
buildDriverSource source caseId =
  case findClassHeader sourceLines of
    Nothing -> Left ("Class not found: " ++ caseIdClassName caseId)
    Just classHeader ->
      case findNamedMethodBlock (caseIdMethodName caseId) of
        Nothing -> Left ("Method not found: " ++ caseIdMethodName caseId)
        Just methodBlock ->
          Right
            ( unlines
                ( importLines
                    ++ [classHeader]
                    ++ setUpBlock
                    ++ methodBlock
                    ++ tearDownBlock
                    ++ driverLines
                )
            )
  where
    sourceLines = lines source
    importLines = filter isImportLine sourceLines

    driverLines =
      [ "__python_hs_case = " ++ caseIdClassName caseId ++ "()",
        "__python_hs_case.setUp()",
        "__python_hs_case." ++ caseIdMethodName caseId ++ "()",
        "__python_hs_case.tearDown()",
        "print(\"__PYTHON_HS_CPYTHON_TEST_OK__\")"
      ]

    setUpBlock
      | caseIdMethodName caseId == "setUp" = []
      | otherwise = maybe [] id (findNamedMethodBlock "setUp")

    tearDownBlock
      | caseIdMethodName caseId == "tearDown" = []
      | otherwise = maybe [] id (findNamedMethodBlock "tearDown")

    isImportLine line =
      "import " `isPrefixOf` line || "from " `isPrefixOf` line

    findClassHeader (line : rest)
      | isClassHeader line = Just line
      | otherwise = findClassHeader rest
    findClassHeader [] = Nothing

    isClassHeader line =
      case stripPrefix ("class " ++ caseIdClassName caseId) line of
        Just (nextChar : _) -> nextChar == '(' || nextChar == ':'
        _ -> False

    findNamedMethodBlock methodName = findMethodBlock sourceLines
      where
        findMethodBlock (line : rest)
          | isMethodStart methodName line = Just (line : takeMethodBody rest)
          | otherwise = findMethodBlock rest
        findMethodBlock [] = Nothing

    isMethodStart methodName line =
      indentation line == 4
        && isPrefixOf ("def " ++ methodName) (drop 4 line)

    takeMethodBody (line : rest)
      | isBlank line = line : takeMethodBody rest
      | indentation line > 4 = line : takeMethodBody rest
      | otherwise = []
    takeMethodBody [] = []

    isBlank = all isSpace

    indentation = length . takeWhile (== ' ')

    stripPrefix prefix text =
      case splitAt (length prefix) text of
        (headPart, tailPart)
          | headPart == prefix -> Just tailPart
        _ -> Nothing
