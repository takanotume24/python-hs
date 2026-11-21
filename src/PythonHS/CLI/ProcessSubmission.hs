module PythonHS.CLI.ProcessSubmission (processSubmission) where

import PythonHS.AST.Program (Program (Program))
import PythonHS.CLI.ProcessSubmissionConfig (ProcessSubmissionConfig (..))
import PythonHS.CLI.SubmissionResult (SubmissionResult (..))
import PythonHS.Evaluator.EvalExpr (evalExpr)
import PythonHS.Evaluator.EvalExprInput (EvalExprInput (..))
import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.EvalStatements (evalStatements)
import PythonHS.Evaluator.EvalStmtResult (EvalStmtResult (..))
import PythonHS.Evaluator.EvalStmtRet (EvalStmtRet (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (BreakValue, ContinueValue, NoneValue))
import PythonHS.Evaluator.ValueToReplOutput (valueToReplOutput)
import PythonHS.Lexer.ScanTokens (scanTokens)
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (EOFToken, NewlineToken))
import PythonHS.Parser.ParseExpr (parseExpr)
import PythonHS.Parser.ParseProgram (parseProgram)
import PythonHS.Parser.ParseResult (ParseResult (..))

processSubmission :: ProcessSubmissionConfig -> Either String SubmissionResult
processSubmission config =
  let env = processSubmissionEnv config
      fenv = processSubmissionFuncEnv config
      src = processSubmissionSrc config
   in case scanTokens src of
        Left lexErr -> Left (show lexErr)
        Right tokens ->
          case parseProgram tokens of
            Left parseErr ->
              case parseReplExpr tokens of
                Just expr ->
                  case evalExpr EvalExprInput {evalExprInputEvalStatementsFn = evalStatements, evalExprInputEnv = env, evalExprInputFuncEnv = fenv, evalExprInputExpr = expr} of
                    Left err -> Left err
                    Right exprResult ->
                      let val = evalExprResultValue exprResult
                          exprOuts = evalExprResultOutputs exprResult
                          envAfterExpr = evalExprResultEnv exprResult
                       in let resultOuts =
                                case val of
                                  NoneValue -> []
                                  _ -> [valueToReplOutput val]
                           in Right SubmissionResult {submissionEnv = envAfterExpr, submissionFuncEnv = fenv, submissionOutputs = exprOuts ++ resultOuts}
                Nothing -> Left (show parseErr)
            Right (Program stmts) ->
              case evalStatements env fenv [] stmts of
                Left err -> Left err
                Right EvalStmtResult {evalStmtResultEnv = env', evalStmtResultFuncEnv = fenv', evalStmtResultOutputs = outs, evalStmtResultRet = mret} ->
                  case mret of
                    Just EvalStmtRet {evalStmtRetValue = BreakValue, evalStmtRetPos = pos} -> Left $ "Break outside loop at " ++ showPos pos
                    Just EvalStmtRet {evalStmtRetValue = ContinueValue, evalStmtRetPos = pos} -> Left $ "Continue outside loop at " ++ showPos pos
                    Just EvalStmtRet {evalStmtRetPos = pos} -> Left $ "Return outside function at " ++ showPos pos
                    Nothing -> Right SubmissionResult {submissionEnv = env', submissionFuncEnv = fenv', submissionOutputs = outs}
  where
    parseReplExpr tokens =
      case parseExpr tokens of
        Right ParseResult {parseResultValue = expr, parseResultRemaining = remaining} | onlyLineEnd remaining -> Just expr
        _ -> Nothing

    onlyLineEnd [Token NewlineToken _ _, Token EOFToken _ _] = True
    onlyLineEnd [Token EOFToken _ _] = True
    onlyLineEnd _ = False
