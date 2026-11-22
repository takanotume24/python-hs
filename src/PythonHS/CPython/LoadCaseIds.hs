module PythonHS.CPython.LoadCaseIds (loadCaseIds) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf)
import PythonHS.CPython.CaseId (CaseId)
import PythonHS.CPython.ParseCaseId (parseCaseId)
import PythonHS.CPython.ReadFileSafely (readFileSafely)

-- | Read and parse every entry in an allowlist file.
loadCaseIds :: FilePath -> IO (Either String [CaseId])
loadCaseIds path = do
  contentsResult <- readFileSafely path
  pure (contentsResult >>= parseEntries)
  where
    parseEntries contents =
      traverse parseEntry (filter isEntry (lines contents))

    isEntry line =
      let trimmed = trim line
       in not (null trimmed) && not ("#" `isPrefixOf` trimmed)

    trim = dropWhile isSpace . dropWhileEnd isSpace

    parseEntry = parseCaseId
