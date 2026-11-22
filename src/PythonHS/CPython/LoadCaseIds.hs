module PythonHS.CPython.LoadCaseIds (loadCaseIds) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf)
import PythonHS.CPython.CaseId (CaseId)
import PythonHS.CPython.ExpandAllowlistEntry (expandAllowlistEntry)
import PythonHS.CPython.ParseAllowlistEntry (parseAllowlistEntry)
import PythonHS.CPython.ReadFileSafely (readFileSafely)

-- | Read, parse, and expand every entry in an allowlist file.
--
-- File and class entries are expanded into their @test*@ methods using the
-- referenced source file under @repoRoot@.
loadCaseIds :: FilePath -> FilePath -> IO (Either String [CaseId])
loadCaseIds repoRoot path = do
  contentsResult <- readFileSafely path
  case contentsResult of
    Left err -> pure (Left err)
    Right contents ->
      case traverse parseAllowlistEntry (filter isEntry (lines contents)) of
        Left err -> pure (Left err)
        Right entries -> do
          expanded <- traverse (expandAllowlistEntry repoRoot) entries
          pure (concat <$> sequence expanded)
  where
    isEntry line =
      let trimmed = trim line
       in not (null trimmed) && not ("#" `isPrefixOf` trimmed)

    trim = dropWhile isSpace . dropWhileEnd isSpace
