module PythonHS.CLI.StartRepl (startRepl) where

import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import System.Console.Haskeline (defaultSettings, getInputLine, outputStrLn, runInputT)
import PythonHS.CLI.ProcessSubmission (processSubmission)
import PythonHS.CLI.ProcessVmSubmission (processVmSubmission)
import PythonHS.CLI.ReplEnvState (ReplEnvState (..))
import PythonHS.CLI.SubmissionResult (SubmissionResult (..))
import PythonHS.CLI.VmSubmissionResult (VmSubmissionResult (..))
import PythonHS.Runner.RunnerEngine (RunnerEngine (AstEngine, VmEngine))
import PythonHS.Runner.ResolveRunnerEngine (resolveRunnerEngine)
import System.Environment (lookupEnv)

startRepl :: IO ()
startRepl = do
  envEngine <- lookupEnv "PYTHON_HS_RUNNER_ENGINE"
  case resolveRunnerEngine envEngine of
    AstEngine -> runInputT defaultSettings (loop (ReplEnvState Map.empty Map.empty) [])
    VmEngine -> runInputT defaultSettings (loopVm [] [] [])
  where
    trimRight = reverse . dropWhile isSpace . reverse
    trim = dropWhile isSpace . trimRight
    endsWithColon s = not (null (trimRight s)) && last (trimRight s) == ':'
    isExitCommand s = trim s == "exit()"

    submitBufferIO state buf =
      let env = replEnvStateEnv state
          fenv = replEnvStateFuncEnv state
          src = unlines buf
       in case processSubmission env fenv src of
            Left err -> outputStrLn ("Error: " ++ err) >> return state
            Right result -> mapM_ outputStrLn (submissionOutputs result) >> return (ReplEnvState (submissionEnv result) (submissionFuncEnv result))

    loop state buf = do
      let env = replEnvStateEnv state
          fenv = replEnvStateFuncEnv state
      mLine <- getInputLine (if null buf then ">>> " else "... ")
      case mLine of
        Nothing -> do
          if null buf
            then return ()
            else do
              _ <- submitBufferIO state buf
              return ()
          outputStrLn ""
        Just line ->
          if null buf && isExitCommand line
            then return ()
            else
              if null buf && trimRight line == ""
                then loop state []
                else
                  if null buf && not (endsWithColon line)
                    then do
                      state' <- case processSubmission env fenv (line ++ "\n") of
                        Left err -> outputStrLn ("Error: " ++ err) >> return state
                        Right result -> mapM_ outputStrLn (submissionOutputs result) >> return (ReplEnvState (submissionEnv result) (submissionFuncEnv result))
                      loop state' []
                    else
                      if not (null buf) && trimRight line == ""
                        then do
                          state' <- submitBufferIO state (init (buf ++ [line]))
                          loop state' []
                        else loop state (buf ++ [line])

    submitVmBufferIO acceptedLines acceptedOutputs buf =
      case processVmSubmission acceptedLines acceptedOutputs buf of
        Left err -> outputStrLn ("Error: " ++ err) >> return (acceptedLines, acceptedOutputs)
        Right result -> mapM_ outputStrLn (vmResultDeltaOutputs result) >> return (vmResultLines result, vmResultOutputs result)

    loopVm acceptedLines acceptedOutputs buf = do
      mLine <- getInputLine (if null buf then ">>> " else "... ")
      case mLine of
        Nothing -> do
          if null buf
            then return ()
            else do
              _ <- submitVmBufferIO acceptedLines acceptedOutputs buf
              return ()
          outputStrLn ""
        Just line ->
          if null buf && isExitCommand line
            then return ()
            else
              if null buf && trimRight line == ""
                then loopVm acceptedLines acceptedOutputs []
                else
                  if null buf && not (endsWithColon line)
                    then do
                      (newLines, newOutputs) <- submitVmBufferIO acceptedLines acceptedOutputs [line]
                      loopVm newLines newOutputs []
                    else
                      if not (null buf) && trimRight line == ""
                        then do
                          (newLines, newOutputs) <- submitVmBufferIO acceptedLines acceptedOutputs (init (buf ++ [line]))
                          loopVm newLines newOutputs []
                        else loopVm acceptedLines acceptedOutputs (buf ++ [line])
