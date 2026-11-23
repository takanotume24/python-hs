module HsLint.CheckStructureWarnings (checkStructureWarnings) where

import Control.Monad (forM)
import Data.List (sortOn)
import Data.Ord (Down (Down))
import HsLint.ExemptFileInfo (ExemptFileInfo (..))
import HsLint.StructureConfig (StructureConfig (..))
import System.Directory (doesFileExist)
import System.FilePath ((</>))

checkStructureWarnings :: StructureConfig -> IO [String]
checkStructureWarnings config = do
  entries <- fmap concat $ forM (structureLineExemptions config) $ \relPath -> do
    let fullPath = structureRoot config </> relPath
    exists <- doesFileExist fullPath
    if exists
      then do
        content <- readFile fullPath
        let lineCount = length (lines content)
        return [ExemptFileInfo {filePathExempt = relPath, lineCountExempt = lineCount}]
      else return []
  let sortedEntries = sortOn (Down . lineCountExempt) entries
  return (map formatWarning sortedEntries)
  where
    formatWarning entry =
      let relPath = filePathExempt entry
          lineCount = lineCountExempt entry
       in "temporary line-limit exemption: " ++ relPath ++ " (" ++ show lineCount ++ " lines)"
