{-# LANGUAGE OverloadedStrings #-}

module HsLint.Config
  ( Config (..),
    defaultConfig,
    decodeConfig,
  )
where

import Data.Aeson (FromJSON (..), decode, withObject, (.!=), (.:?))
import Data.ByteString.Lazy (ByteString)
import HsLint.PositionalArgsConfig (PositionalArgsConfig, defaultPositionalArgsConfig)
import HsLint.StructureConfig (StructureConfig, defaultStructureConfig)

-- | Top-level configuration for every rule.
data Config = Config
  { configStructure :: StructureConfig,
    configPositionalArgs :: PositionalArgsConfig
  }
  deriving (Eq, Show)

-- | Configuration used when no config file is supplied.
defaultConfig :: Config
defaultConfig =
  Config
    { configStructure = defaultStructureConfig,
      configPositionalArgs = defaultPositionalArgsConfig
    }

-- | Decode a JSON config document, filling missing fields with defaults.
decodeConfig :: ByteString -> Maybe Config
decodeConfig = decode

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    structure <- o .:? "structure" .!= defaultStructureConfig
    positionalArgs <- o .:? "positionalArgs" .!= defaultPositionalArgsConfig
    pure
      Config
        { configStructure = structure,
          configPositionalArgs = positionalArgs
        }
