module PythonHS.CLI.ReplEvalLines (replEvalLines) where

import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import PythonHS.CLI.ProcessSubmission (processSubmission)

replEvalLines :: [String] -> IO [String]
replEvalLines inputs = go inputs Map.empty Map.empty [] []
  where
    trimRight = reverse . dropWhile isSpace . reverse
    trim = dropWhile isSpace . trimRight
    endsWithColon s = not (null (trimRight s)) && last (trimRight s) == ':'
    isExitCommand s = trim s == "exit()"

    submitBuffer env fenv buf outsAcc =
      let src = unlines buf
       in case processSubmission env fenv src of
            Left err -> (env, fenv, outsAcc ++ ["Error: " ++ err])
            Right (env', fenv', outs) -> (env', fenv', outsAcc ++ outs)

    go [] _ _ [] outsAcc = return outsAcc
    go [] env fenv buf outsAcc =
      let (_, _, outsAcc') = submitBuffer env fenv buf outsAcc
       in return outsAcc'
    go (ln : rest) env fenv [] outsAcc
      | isExitCommand ln = return outsAcc
      | trimRight ln == "" = go rest env fenv [] outsAcc
      | endsWithColon ln = go rest env fenv [ln] outsAcc
      | otherwise =
        case processSubmission env fenv (ln ++ "\n") of
          Left err -> go rest env fenv [] (outsAcc ++ ["Error: " ++ err])
          Right (env', fenv', outs) -> go rest env' fenv' [] (outsAcc ++ outs)
    go (ln : rest) env fenv buf outsAcc
      | trimRight ln == "" =
        let (env', fenv', outsAcc') = submitBuffer env fenv buf outsAcc
         in go rest env' fenv' [] outsAcc'
      | otherwise = go rest env fenv (buf ++ [ln]) outsAcc