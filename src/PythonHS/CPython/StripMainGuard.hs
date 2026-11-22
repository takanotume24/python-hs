module PythonHS.CPython.StripMainGuard (stripMainGuard) where

import Data.Char (isSpace)
import Data.List (isPrefixOf)

-- | Remove a top-level @if __name__ == "__main__":@ block.
--
-- Whole-file execution appends its own driver, so the test runner's
-- @unittest.main()@ entry point must not be executed.
stripMainGuard :: String -> String
stripMainGuard source = unlines (go (lines source))
  where
    go allLines =
      case break isMainGuard allLines of
        (before, []) -> before
        (before, _mainGuardLine : rest) -> before ++ dropIndentedBlock rest

    dropIndentedBlock allLines =
      case allLines of
        [] -> []
        line : rest
          | isBlank line -> line : dropIndentedBlock rest
          | indentation line > 0 -> dropIndentedBlock rest
          | otherwise -> allLines

    isMainGuard line =
      indentation line == 0 && "if __name__" `isPrefixOf` line

    isBlank = all isSpace

    indentation = length . takeWhile (== ' ')
