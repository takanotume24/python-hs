module PythonHS.CPython.ExpandAllowlistEntry (expandAllowlistEntry) where

import PythonHS.CPython.AllowlistEntry (AllowlistEntry (..))
import PythonHS.CPython.CaseId (CaseId (..))
import PythonHS.CPython.ExtractTestCases (extractTestCases)
import PythonHS.CPython.ReadFileSafely (readFileSafely)
import System.FilePath ((</>))

-- | Expand a file or class entry by scanning the referenced source file.
expandAllowlistEntry :: FilePath -> AllowlistEntry -> IO (Either String [CaseId])
expandAllowlistEntry repoRoot entry =
  case (allowlistEntryClassName entry, allowlistEntryMethodName entry) of
    (Just className, Just methodName) ->
      pure (Right [methodCase className methodName])
    (Just className, Nothing) -> do
      sourceResult <- readFileSafely (repoRoot </> allowlistEntryFile entry)
      pure $ do
        source <- sourceResult
        let cases = filter ((== className) . caseIdClassName) (extractTestCases (allowlistEntryFile entry) source)
        if null cases
          then Left ("Class not found or has no test methods: " ++ className)
          else Right cases
    (Nothing, Nothing) -> do
      sourceResult <- readFileSafely (repoRoot </> allowlistEntryFile entry)
      pure (fmap (extractTestCases (allowlistEntryFile entry)) sourceResult)
    (Nothing, Just methodName) ->
      pure (Right [methodCase "" methodName])
  where
    methodCase className methodName =
      CaseId
        { caseIdFile = allowlistEntryFile entry,
          caseIdClassName = className,
          caseIdMethodName = methodName
        }
