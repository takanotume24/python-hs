module Main (main) where

import PythonHS.Lexer.ScanTokens (scanTokens)
import PythonHS.Parser.ParseProgram (parseProgram)
import PythonHS.VM.CompileProgram (compileProgram)
import PythonHS.VM.Instruction (Instruction)
import PythonHS.VM.RunInstructionsTrace (runInstructionsTrace)
import PythonHS.VM.TraceRunResult (TraceRunResult (..))
import PythonHS.VM.TraceStep (TraceStep (..))
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [sourcePath] -> traceRunInFile sourcePath
    _ -> do
      putStrLn "usage: trace-vm <source file>"
      exitFailure
  where
    traceRunInFile path = do
      source <- readFile path
      case scanTokens source of
        Left errorResult -> putStrLn ("lex error: " ++ show errorResult)
        Right tokens ->
          case parseProgram tokens of
            Left errorResult -> putStrLn ("parse error: " ++ show errorResult)
            Right program ->
              case compileProgram program of
                Left errorResult -> putStrLn ("compile error: " ++ errorResult)
                Right instructions -> printTraceRun instructions

    printTraceRun :: [Instruction] -> IO ()
    printTraceRun instructions = do
      putStrLn "== instructions =="
      mapM_ (\indexedInstruction -> putStrLn (show (fst indexedInstruction) ++ ": " ++ show (snd indexedInstruction))) (zip ([0 ..] :: [Int]) instructions)
      putStrLn ""
      putStrLn "== trace =="
      let traceRun = runInstructionsTrace instructions
      mapM_ printStep (traceRunResultSteps traceRun)
      putStrLn ""
      putStrLn "== result =="
      case traceRunResultFailure traceRun of
        Just failureMessage -> putStrLn ("failure: " ++ failureMessage)
        Nothing -> putStrLn "failure: (none)"
      putStrLn ("outputs: " ++ show (traceRunResultOutputs traceRun))

    printStep :: TraceStep -> IO ()
    printStep step =
      putStrLn
        ( "ip="
            ++ show (traceStepIp step)
            ++ " handlers="
            ++ show (traceStepHandlers step)
            ++ " stack="
            ++ show (traceStepStack step)
            ++ " :: "
            ++ show (traceStepInstruction step)
        )
