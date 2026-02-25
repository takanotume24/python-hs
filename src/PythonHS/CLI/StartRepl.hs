module PythonHS.CLI.StartRepl (startRepl) where

import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import System.Console.Haskeline (defaultSettings, getInputLine, outputStrLn, runInputT)
import PythonHS.CLI.ProcessSubmission (processSubmission)

startRepl :: IO ()
startRepl = runInputT defaultSettings (loop Map.empty Map.empty [])
  where
    trimRight = reverse . dropWhile isSpace . reverse
    endsWithColon s = not (null (trimRight s)) && last (trimRight s) == ':'

    submitBufferIO env fenv buf =
      let src = unlines buf
       in case processSubmission env fenv src of
            Left err -> outputStrLn ("Error: " ++ err) >> return (env, fenv)
            Right (env', fenv', outs) -> mapM_ outputStrLn outs >> return (env', fenv')

    loop env fenv buf = do
      mLine <- getInputLine (if null buf then ">>> " else "... ")
      case mLine of
        Nothing -> do
          if null buf
            then return ()
            else do
              _ <- submitBufferIO env fenv buf
              return ()
          outputStrLn ""
        Just line ->
          if null buf && trimRight line == ""
            then loop env fenv []
            else
              if null buf && not (endsWithColon line)
                then do
                  (env', fenv') <- case processSubmission env fenv (line ++ "\n") of
                    Left err -> outputStrLn ("Error: " ++ err) >> return (env, fenv)
                    Right (env'', fenv'', outs) -> mapM_ outputStrLn outs >> return (env'', fenv'')
                  loop env' fenv' []
                else
                  if not (null buf) && trimRight line == ""
                    then do
                      (env', fenv') <- submitBufferIO env fenv (init (buf ++ [line]))
                      loop env' fenv' []
                    else loop env fenv (buf ++ [line])