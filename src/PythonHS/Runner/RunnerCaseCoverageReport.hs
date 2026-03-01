module PythonHS.Runner.RunnerCaseCoverageReport (runnerCaseCoverageReport) where

import Data.List (sort)

runnerCaseCoverageReport :: FilePath -> FilePath -> FilePath -> IO String
runnerCaseCoverageReport edgePath parityPath vmPath = do
  edgeSource <- readFile edgePath
  paritySource <- readFile parityPath
  vmSource <- readFile vmPath
  let edgeCases = uniqueSortedCases (extractQuotedAfterPrefix "runSource \"" edgeSource)
  let parityCases = uniqueSortedCases (extractQuotedAfterPrefix "shouldMatchVm \"" paritySource)
  let vmCases = uniqueSortedCases (extractQuotedAfterPrefix "runSourceVm \"" vmSource)
  let missingParity = listDifference edgeCases parityCases
  let missingVm = listDifference edgeCases vmCases
  pure (renderReport missingParity missingVm)
  where
    extractQuotedAfterPrefix prefix source =
      go source
      where
        go remaining =
          case findPrefix prefix remaining of
            Nothing -> []
            Just afterPrefix ->
              case takeQuotedLiteral afterPrefix of
                Nothing -> []
                Just (literal, rest) -> literal : go rest

        findPrefix needle haystack
          | startsWith needle haystack = Just (drop (length needle) haystack)
          | null haystack = Nothing
          | otherwise =
              case haystack of
                [] -> Nothing
                _ : rest -> findPrefix needle rest

        startsWith [] _ = True
        startsWith _ [] = False
        startsWith (x : xs) (y : ys) = x == y && startsWith xs ys

    takeQuotedLiteral text =
      parseLiteral text []
      where
        parseLiteral [] _ = Nothing
        parseLiteral ('"' : rest) acc = Just (reverse acc, rest)
        parseLiteral ('\\' : escaped : rest) acc = parseLiteral rest (escaped : '\\' : acc)
        parseLiteral (_ : []) _ = Nothing
        parseLiteral (ch : rest) acc = parseLiteral rest (ch : acc)

    uniqueSortedCases = collapse . sort
      where
        collapse items =
          case items of
            [] -> []
            x : xs -> x : collapse (dropWhile (== x) xs)

    listDifference left right =
      case (left, right) of
        ([], _) -> []
        (items, []) -> items
        (l : ls, r : rs)
          | l == r -> listDifference ls rs
          | l < r -> l : listDifference ls (r : rs)
          | otherwise -> listDifference (l : ls) rs

    renderReport missingParity missingVm =
      unlines
        ( ["=== MISSING IN PARITY ==="]
            ++ missingParity
            ++ ["=== COUNT PARITY ===", show (length missingParity), "=== MISSING IN VM ==="]
            ++ missingVm
            ++ ["=== COUNT VM ===", show (length missingVm)]
        )
