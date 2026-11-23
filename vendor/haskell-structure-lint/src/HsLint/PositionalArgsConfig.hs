{-# LANGUAGE OverloadedStrings #-}

module HsLint.PositionalArgsConfig
  ( PositionalArgsConfig (..),
    defaultPositionalArgsConfig,
  )
where

import Data.Aeson (FromJSON (..), withObject, (.!=), (.:?))

-- | Rules for the positional-argument checker.
data PositionalArgsConfig = PositionalArgsConfig
  { positionalExcludes :: [String],
    positionalSkipFunctionPaths :: [String]
  }
  deriving (Eq, Show)

-- | By default nothing is excluded and no path is skipped.
defaultPositionalArgsConfig :: PositionalArgsConfig
defaultPositionalArgsConfig =
  PositionalArgsConfig
    { positionalExcludes = [],
      positionalSkipFunctionPaths = []
    }

instance FromJSON PositionalArgsConfig where
  parseJSON = withObject "PositionalArgsConfig" $ \o -> do
    let d = defaultPositionalArgsConfig
    excludes <- o .:? "excludes" .!= positionalExcludes d
    skipFunctionPaths <- o .:? "skipFunctionPaths" .!= positionalSkipFunctionPaths d
    pure
      PositionalArgsConfig
        { positionalExcludes = excludes,
          positionalSkipFunctionPaths = skipFunctionPaths
        }
