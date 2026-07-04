module PythonHS.Structure.CheckStructureWarnings (checkStructureWarnings) where

import Control.Monad (forM)
import Data.List (sortOn)
import Data.Ord (Down (Down))
import PythonHS.Structure.ExemptFileInfo (ExemptFileInfo (..))
import System.Directory (doesFileExist)
import System.FilePath ((</>))

checkStructureWarnings :: FilePath -> IO [String]
checkStructureWarnings root = do
  entries <- fmap concat $ forM legacyLineCountExemptions $ \relPath -> do
    let fullPath = root </> relPath
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

    legacyLineCountExemptions = []
