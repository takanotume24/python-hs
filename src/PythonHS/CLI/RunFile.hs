module PythonHS.CLI.RunFile (runFile) where

import Control.Exception (IOException, try)
import PythonHS.Runner (runSource)

runFile :: FilePath -> IO (Either String [String])
runFile path = do
  eres <- try (readFile path) :: IO (Either IOException String)
  case eres of
    Left e -> return $ Left (show e)
    Right contents -> return $ runSource contents