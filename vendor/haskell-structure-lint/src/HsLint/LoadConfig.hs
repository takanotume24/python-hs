module HsLint.LoadConfig (loadConfig) where

import Data.ByteString.Lazy qualified as BL
import HsLint.Config (Config, decodeConfig, defaultConfig)
import System.Directory (doesFileExist)

-- | Load a config file. When no path is given, use @structure-lint.json@ in the
-- current directory if it exists, otherwise fall back to the defaults.
loadConfig :: Maybe FilePath -> IO Config
loadConfig explicitPath = do
  selectedPath <- case explicitPath of
    Just path -> pure (Just path)
    Nothing -> do
      exists <- doesFileExist "structure-lint.json"
      pure (if exists then Just "structure-lint.json" else Nothing)
  case selectedPath of
    Nothing -> pure defaultConfig
    Just path -> do
      bytes <- BL.readFile path
      case decodeConfig bytes of
        Nothing -> ioError (userError ("Invalid structure-lint config: " ++ path))
        Just config -> pure config
