module PythonHS.CPython.ExtractTestCases (extractTestCases) where

import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf)
import PythonHS.CPython.CaseId (CaseId (..))

-- | Collect every @test*@ method declared directly inside a class.
extractTestCases :: FilePath -> String -> [CaseId]
extractTestCases file source =
  concatMap casesOfClass (classBlocks (lines source))
  where
    casesOfClass (className, classBody) =
      [ CaseId
          { caseIdFile = file,
            caseIdClassName = className,
            caseIdMethodName = methodName
          }
      | methodName <- testMethodNames classBody
      ]

    classBlocks [] = []
    classBlocks (line : rest)
      | isClassHeader line =
          let (classBody, remaining) = takeClassBody rest
           in (classNameOf line, classBody) : classBlocks remaining
      | otherwise = classBlocks rest

    takeClassBody (line : rest)
      | isBlank line =
          let (classBody, remaining) = takeClassBody rest
           in (line : classBody, remaining)
      | indentation line > 0 =
          let (classBody, remaining) = takeClassBody rest
           in (line : classBody, remaining)
      | otherwise = ([], line : rest)
    takeClassBody [] = ([], [])

    isClassHeader line =
      indentation line == 0 && "class " `isPrefixOf` line

    classNameOf line =
      takeWhile isIdentifierChar (drop (length "class ") line)

    testMethodNames classBody =
      [ methodName
      | line <- classBody,
        indentation line == 4,
        Just methodName <- [methodNameOf line],
        "test" `isPrefixOf` methodName
      ]

    methodNameOf line =
      case stripPrefix "def " (drop 4 line) of
        Just afterDef ->
          let methodName = takeWhile isIdentifierChar afterDef
           in if null methodName then Nothing else Just methodName
        Nothing -> Nothing

    isIdentifierChar character =
      isAlphaNum character || character == '_'

    isBlank = all isSpace

    indentation = length . takeWhile (== ' ')

    stripPrefix prefix text =
      case splitAt (length prefix) text of
        (headPart, tailPart)
          | headPart == prefix -> Just tailPart
        _ -> Nothing
