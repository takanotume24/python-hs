module PythonHS.CLI.StartRepl (startRepl) where

import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import System.IO (hFlush, isEOF, stdout)
import PythonHS.CLI.ProcessSubmission (processSubmission)

startRepl :: IO ()
startRepl = loop Map.empty Map.empty []
  where
    trimRight = reverse . dropWhile isSpace . reverse
    endsWithColon s = not (null (trimRight s)) && last (trimRight s) == ':'

    submitBufferIO env fenv buf =
      let src = unlines buf
       in case processSubmission env fenv src of
            Left err -> putStrLn ("Error: " ++ err) >> return (env, fenv)
            Right (env', fenv', outs) -> mapM_ putStrLn outs >> return (env', fenv')

    loop env fenv buf = do
      end <- isEOF
      if end
        then putStrLn ""
        else do
          putStr (if null buf then ">>> " else "... ")
          hFlush stdout
          line <- getLine
          if null buf && trimRight line == ""
            then loop env fenv []
            else
              if null buf && not (endsWithColon line)
                then do
                  (env', fenv') <- case processSubmission env fenv (line ++ "\n") of
                    Left err -> putStrLn ("Error: " ++ err) >> return (env, fenv)
                    Right (env'', fenv'', outs) -> mapM_ putStrLn outs >> return (env'', fenv'')
                  loop env' fenv' []
                else
                  if not (null buf) && trimRight line == ""
                    then do
                      (env', fenv') <- submitBufferIO env fenv (init (buf ++ [line]))
                      loop env' fenv' []
                    else loop env fenv (buf ++ [line])