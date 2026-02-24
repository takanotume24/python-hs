module PythonHS.CLI.ProcessSubmission (processSubmission) where

import PythonHS.AST.Program (Program (Program))
import PythonHS.Evaluator.EvalStatements (evalStatements)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (BreakValue, ContinueValue))
import PythonHS.Lexer.ScanTokens (scanTokens)
import PythonHS.Parser.ParseProgram (parseProgram)

processSubmission :: Env -> FuncEnv -> String -> Either String (Env, FuncEnv, [String])
processSubmission env fenv src =
  case scanTokens src of
    Left lexErr -> Left (show lexErr)
    Right tokens ->
      case parseProgram tokens of
        Left parseErr -> Left (show parseErr)
        Right (Program stmts) ->
          case evalStatements env fenv [] stmts of
            Left err -> Left err
            Right (env', fenv', outs, mret) ->
              case mret of
                Just (BreakValue, pos) -> Left $ "Break outside loop at " ++ showPos pos
                Just (ContinueValue, pos) -> Left $ "Continue outside loop at " ++ showPos pos
                Just (_, pos) -> Left $ "Return outside function at " ++ showPos pos
                Nothing -> Right (env', fenv', outs)