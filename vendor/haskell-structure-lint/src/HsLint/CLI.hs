module HsLint.CLI (runCheckCommand, runPositionalCommand) where

import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import HsLint.CheckStructure (checkStructureViolations, checkStructureWarnings)
import HsLint.Config (Config (..))
import HsLint.DetectPositionalArgs (detectPositionalArgs)
import HsLint.DetectPositionalArgsFromDirectory (detectPositionalArgsFromDirectory)
import HsLint.DetectPositionalArgsFromDirectoryConfig (DetectPositionalArgsFromDirectoryConfig (..))
import HsLint.FormatViolationsJson (formatViolationsJson)
import HsLint.FormatViolationsPlain (formatViolationsPlain)
import HsLint.LoadConfig (loadConfig)
import HsLint.PositionalArgsConfig (PositionalArgsConfig (..))
import HsLint.StructureConfig (StructureConfig (..))
import System.Directory (doesDirectoryExist)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

-- | Options accepted by the @check@ subcommand.
data CheckArgs = CheckArgs
  { checkArgsConfigPath :: Maybe FilePath,
    checkArgsRoot :: Maybe FilePath,
    checkArgsMaxLines :: Maybe Int
  }

emptyCheckArgs :: CheckArgs
emptyCheckArgs =
  CheckArgs
    { checkArgsConfigPath = Nothing,
      checkArgsRoot = Nothing,
      checkArgsMaxLines = Nothing
    }

-- | Options accepted by the @positional@ subcommand.
data PositionalCommandArgs = PositionalCommandArgs
  { positionalCommandArgsConfigPath :: Maybe FilePath,
    positionalCommandArgsTarget :: Maybe FilePath,
    positionalCommandArgsJson :: Bool,
    positionalCommandArgsExcludes :: [String]
  }

emptyPositionalCommandArgs :: PositionalCommandArgs
emptyPositionalCommandArgs =
  PositionalCommandArgs
    { positionalCommandArgsConfigPath = Nothing,
      positionalCommandArgsTarget = Nothing,
      positionalCommandArgsJson = False,
      positionalCommandArgsExcludes = []
    }

runCheckCommand :: [String] -> IO ()
runCheckCommand args =
  case parseCheckArgs args of
    Nothing -> do
      putStrLn "Usage: check-structure [--config FILE] [--root DIR] [--max-lines N]"
      exitFailure
    Just checkArgs -> do
      config <- loadConfig (checkArgsConfigPath checkArgs)
      let structure = applyStructureOverrides checkArgs (configStructure config)
      warnings <- checkStructureWarnings structure
      if null warnings
        then pure ()
        else do
          putStrLn "Structure check warnings:"
          mapM_ (putStrLn . ("- " ++)) warnings
      violations <- checkStructureViolations structure
      if null violations
        then putStrLn "Structure check passed"
        else do
          putStrLn "Structure check failed:"
          mapM_ (putStrLn . ("- " ++)) violations
          exitFailure

runPositionalCommand :: [String] -> IO ()
runPositionalCommand args =
  case parsePositionalCommandArgs args of
    Nothing -> do
      putStrLn "Usage: detect-positional-args [--config FILE] [--json] [--exclude PATTERN]... [PATH]"
      exitFailure
    Just parsed -> do
      config <- loadConfig (positionalCommandArgsConfigPath parsed)
      let positional = applyPositionalOverrides parsed (configPositionalArgs config)
          target = fromMaybe "." (positionalCommandArgsTarget parsed)
          formatter = if positionalCommandArgsJson parsed then formatViolationsJson else formatViolationsPlain
      isDirectory <- doesDirectoryExist target
      violations <-
        if isDirectory
          then
            detectPositionalArgsFromDirectory
              DetectPositionalArgsFromDirectoryConfig
                { detectPositionalArgsFromDirectoryDir = target,
                  detectPositionalArgsFromDirectoryExcludes = positionalExcludes positional,
                  detectPositionalArgsFromDirectorySkipFunctionPaths = positionalSkipFunctionPaths positional
                }
          else detectPositionalArgs positional target
      putStrLn (formatter violations)
      unless (null violations) exitFailure

applyStructureOverrides :: CheckArgs -> StructureConfig -> StructureConfig
applyStructureOverrides checkArgs structure =
  structure
    { structureRoot = fromMaybe (structureRoot structure) (checkArgsRoot checkArgs),
      structureMaxLines = fromMaybe (structureMaxLines structure) (checkArgsMaxLines checkArgs)
    }

applyPositionalOverrides :: PositionalCommandArgs -> PositionalArgsConfig -> PositionalArgsConfig
applyPositionalOverrides parsed positional =
  positional
    { positionalExcludes = positionalExcludes positional ++ positionalCommandArgsExcludes parsed
    }

parseCheckArgs :: [String] -> Maybe CheckArgs
parseCheckArgs = go emptyCheckArgs
  where
    go acc [] = Just acc
    go acc ("--config" : path : rest) = go acc {checkArgsConfigPath = Just path} rest
    go acc ("--root" : path : rest) = go acc {checkArgsRoot = Just path} rest
    go acc ("--max-lines" : value : rest) = do
      maxLines <- readMaybe value
      go acc {checkArgsMaxLines = Just maxLines} rest
    go _ _ = Nothing

parsePositionalCommandArgs :: [String] -> Maybe PositionalCommandArgs
parsePositionalCommandArgs = go emptyPositionalCommandArgs
  where
    go acc [] = Just acc
    go acc ("--config" : path : rest) = go acc {positionalCommandArgsConfigPath = Just path} rest
    go acc ("--json" : rest) = go acc {positionalCommandArgsJson = True} rest
    go acc ("--exclude" : pattern' : rest) =
      go acc {positionalCommandArgsExcludes = positionalCommandArgsExcludes acc ++ [pattern']} rest
    go acc ("--" : rest) = case (positionalCommandArgsTarget acc, rest) of
      (Nothing, [path]) -> Just acc {positionalCommandArgsTarget = Just path}
      (_, []) -> Just acc
      _ -> Nothing
    go acc (arg : rest)
      | take 2 arg == "--" = Nothing
      | otherwise = case positionalCommandArgsTarget acc of
          Nothing -> go acc {positionalCommandArgsTarget = Just arg} rest
          Just _ -> Nothing
