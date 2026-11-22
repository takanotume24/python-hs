module PythonHS.CPython.ParseCaseId (parseCaseId) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import PythonHS.CPython.CaseId (CaseId (..))

-- | Parse an allowlist entry such as @path/to/test.py::Class::method@.
parseCaseId :: String -> Either String CaseId
parseCaseId raw =
  case splitOnDoubleColon (trim raw) of
    [file, className, methodName]
      | not (null file),
        not (null className),
        not (null methodName) ->
          Right
            CaseId
              { caseIdFile = file,
                caseIdClassName = className,
                caseIdMethodName = methodName
              }
    _ -> Left ("Invalid test case identifier: " ++ raw)
  where
    trim = dropWhile isSpace . dropWhileEnd isSpace

    splitOnDoubleColon text =
      case breakOnDoubleColon text of
        (headPart, "") -> [headPart]
        (headPart, rest) -> headPart : splitOnDoubleColon (drop 2 rest)

    breakOnDoubleColon text =
      case text of
        ':' : ':' : rest -> ("", ':' : ':' : rest)
        character : rest ->
          let (headPart, tailPart) = breakOnDoubleColon rest
           in (character : headPart, tailPart)
        [] -> ("", "")
