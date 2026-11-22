module PythonHS.CPython.ReadFileSafely (readFileSafely) where

import Control.Exception (IOException, try)

-- | Read a text file, returning a readable error instead of throwing.
readFileSafely :: FilePath -> IO (Either String String)
readFileSafely path = do
  result <- try (readFile path) :: IO (Either IOException String)
  pure (either (Left . show) Right result)
