module PythonHS.VM.ResolveTargetModulePath (resolveTargetModulePath) where

import PythonHS.VM.ResolveTargetModulePathConfig (ResolveTargetModulePathConfig (..))

resolveTargetModulePath :: ResolveTargetModulePathConfig -> Either String [String]
resolveTargetModulePath config = resolve
  where
    currentPackage = resolveTargetModulePathCurrentPackage config
    relativeLevel = resolveTargetModulePathRelativeLevel config
    modulePath = resolveTargetModulePathModulePath config
    resolve
      | relativeLevel <= 0 = Right modulePath
      | null currentPackage = Left "Import error: relative import with no known parent package"
      | otherwise =
          let upLevels = relativeLevel - 1
              parentPath = dropLastN upLevels currentPackage
           in if upLevels > length currentPackage
                then Left "Import error: relative import beyond top-level package"
                else Right (parentPath ++ modulePath)
    dropLastN count segments
      | count <= 0 = segments
      | otherwise = dropLastN (count - 1) (dropLast segments)

    dropLast segments =
      case segments of
        [] -> []
        [_] -> []
        x : xs -> x : dropLast xs
