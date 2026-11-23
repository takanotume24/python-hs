{-# LANGUAGE OverloadedStrings #-}

module HsLint.StructureConfig
  ( StructureConfig (..),
    defaultStructureConfig,
  )
where

import Data.Aeson (FromJSON (..), withObject, (.!=), (.:?))

-- | Rules for the file/module structure checker.
data StructureConfig = StructureConfig
  { structureRoot :: FilePath,
    structureRoots :: [FilePath],
    structureMaxLines :: Int,
    structureLineExemptions :: [FilePath],
    structureTestDirPrefixes :: [FilePath],
    structureAppDirPrefixes :: [FilePath],
    structureBasenameModulePrefixes :: [FilePath],
    structureDiscoverDriver :: Maybe FilePath,
    structureTestSuffix :: String,
    structureModuleRootPrefixes :: [FilePath]
  }
  deriving (Eq, Show)

-- | Defaults that reproduce the original python-hs rules.
defaultStructureConfig :: StructureConfig
defaultStructureConfig =
  StructureConfig
    { structureRoot = ".",
      structureRoots = ["src", "app", "test"],
      structureMaxLines = 200,
      structureLineExemptions = [],
      structureTestDirPrefixes = ["test/"],
      structureAppDirPrefixes = ["app/"],
      structureBasenameModulePrefixes = ["app/"],
      structureDiscoverDriver = Just "test/Spec.hs",
      structureTestSuffix = "Spec",
      structureModuleRootPrefixes = ["src", "app", "test"]
    }

instance FromJSON StructureConfig where
  parseJSON = withObject "StructureConfig" $ \o -> do
    let d = defaultStructureConfig
    root <- o .:? "root" .!= structureRoot d
    roots <- o .:? "roots" .!= structureRoots d
    maxLines <- o .:? "maxLines" .!= structureMaxLines d
    lineExemptions <- o .:? "lineExemptions" .!= structureLineExemptions d
    testDirPrefixes <- o .:? "testDirPrefixes" .!= structureTestDirPrefixes d
    appDirPrefixes <- o .:? "appDirPrefixes" .!= structureAppDirPrefixes d
    basenameModulePrefixes <- o .:? "basenameModulePrefixes" .!= structureBasenameModulePrefixes d
    discoverDriver <- o .:? "discoverDriver" .!= structureDiscoverDriver d
    testSuffix <- o .:? "testSuffix" .!= structureTestSuffix d
    moduleRootPrefixes <- o .:? "moduleRootPrefixes" .!= structureModuleRootPrefixes d
    pure
      StructureConfig
        { structureRoot = root,
          structureRoots = roots,
          structureMaxLines = maxLines,
          structureLineExemptions = lineExemptions,
          structureTestDirPrefixes = testDirPrefixes,
          structureAppDirPrefixes = appDirPrefixes,
          structureBasenameModulePrefixes = basenameModulePrefixes,
          structureDiscoverDriver = discoverDriver,
          structureTestSuffix = testSuffix,
          structureModuleRootPrefixes = moduleRootPrefixes
        }
