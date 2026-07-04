module PythonHS.CLI.ReplEvalLines (replEvalLines) where

import Data.Char (isSpace)
import Data.Map.Strict qualified as Map
import PythonHS.CLI.ProcessSubmission (processSubmission)
import PythonHS.CLI.ProcessVmSubmission (processVmSubmission)
import PythonHS.CLI.ReplEvalState (ReplEvalState (..))
import PythonHS.CLI.SubmissionResult (SubmissionResult (..))
import PythonHS.CLI.VmReplState (VmReplState (..))
import PythonHS.CLI.VmSubmissionResult (VmSubmissionResult (..))
import PythonHS.Runner.ResolveRunnerEngine (resolveRunnerEngine)
import PythonHS.Runner.RunnerEngine (RunnerEngine (AstEngine, VmEngine))
import System.Environment (lookupEnv)

replEvalLines :: [String] -> IO [String]
replEvalLines inputs = do
  envEngine <- lookupEnv "PYTHON_HS_RUNNER_ENGINE"
  case resolveRunnerEngine envEngine of
    AstEngine -> go inputs Map.empty Map.empty [] []
    VmEngine -> goVm inputs [] [] [] []
  where
    trimRight = reverse . dropWhile isSpace . reverse
    trim = dropWhile isSpace . trimRight
    endsWithColon s = not (null (trimRight s)) && last (trimRight s) == ':'
    isExitCommand s = trim s == "exit()"

    submitBuffer env fenv buf outsAcc =
      let src = unlines buf
       in case processSubmission env fenv src of
            Left err -> ReplEvalState {replEnv = env, replFunctionEnv = fenv, replOutputs = outsAcc ++ ["Error: " ++ err]}
            Right result -> ReplEvalState {replEnv = submissionEnv result, replFunctionEnv = submissionFuncEnv result, replOutputs = outsAcc ++ submissionOutputs result}

    go [] _ _ [] outsAcc = return outsAcc
    go [] env fenv buf outsAcc =
      let result = submitBuffer env fenv buf outsAcc
       in return (replOutputs result)
    go (ln : rest) env fenv [] outsAcc
      | isExitCommand ln = return outsAcc
      | trimRight ln == "" = go rest env fenv [] outsAcc
      | endsWithColon ln = go rest env fenv [ln] outsAcc
      | otherwise =
          case processSubmission env fenv (ln ++ "\n") of
            Left err -> go rest env fenv [] (outsAcc ++ ["Error: " ++ err])
            Right result -> go rest (submissionEnv result) (submissionFuncEnv result) [] (outsAcc ++ submissionOutputs result)
    go (ln : rest) env fenv buf outsAcc
      | trimRight ln == "" =
          let result = submitBuffer env fenv buf outsAcc
           in go rest (replEnv result) (replFunctionEnv result) [] (replOutputs result)
      | otherwise = go rest env fenv (buf ++ [ln]) outsAcc

    submitVmBuffer acceptedLines acceptedOutputs buf outsAcc =
      case processVmSubmission acceptedLines acceptedOutputs buf of
        Left err -> VmReplState {vmLines = acceptedLines, vmOutputs = acceptedOutputs, vmAcc = outsAcc ++ ["Error: " ++ err]}
        Right result -> VmReplState {vmLines = vmResultLines result, vmOutputs = vmResultOutputs result, vmAcc = outsAcc ++ vmResultDeltaOutputs result}

    goVm [] _ _ [] outsAcc = return outsAcc
    goVm [] acceptedLines acceptedOutputs buf outsAcc =
      let result = submitVmBuffer acceptedLines acceptedOutputs buf outsAcc
       in return (vmAcc result)
    goVm (ln : rest) acceptedLines acceptedOutputs [] outsAcc
      | isExitCommand ln = return outsAcc
      | trimRight ln == "" = goVm rest acceptedLines acceptedOutputs [] outsAcc
      | endsWithColon ln = goVm rest acceptedLines acceptedOutputs [ln] outsAcc
      | otherwise =
          let result = submitVmBuffer acceptedLines acceptedOutputs [ln] outsAcc
           in goVm rest (vmLines result) (vmOutputs result) [] (vmAcc result)
    goVm (ln : rest) acceptedLines acceptedOutputs buf outsAcc
      | trimRight ln == "" =
          let result = submitVmBuffer acceptedLines acceptedOutputs buf outsAcc
           in goVm rest (vmLines result) (vmOutputs result) [] (vmAcc result)
      | otherwise = goVm rest acceptedLines acceptedOutputs (buf ++ [ln]) outsAcc
