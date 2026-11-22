module PythonHS.CLI.ProcessVmSubmission (processVmSubmission) where

import Data.List (stripPrefix)
import PythonHS.AST.Expr (Expr (CallExpr, CallValueExpr, IdentifierExpr), identifierExprName)
import PythonHS.CLI.ProcessVmSubmissionConfig (ProcessVmSubmissionConfig (..))
import PythonHS.CLI.VmSubmissionResult (VmSubmissionResult (..))
import PythonHS.Lexer.ScanTokens (scanTokens)
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (EOFToken, NewlineToken))
import PythonHS.Parser.ParseExpr (parseExpr)
import PythonHS.Parser.ParseResult (ParseResult (..))
import PythonHS.RunSourceVm (runSourceVm)

processVmSubmission :: ProcessVmSubmissionConfig -> Either String VmSubmissionResult
processVmSubmission config =
  let acceptedSourceLines = processVmSubmissionAcceptedSourceLines config
      acceptedOutputs = processVmSubmissionAcceptedOutputs config
      submissionLines = processVmSubmissionSubmissionLines config
      candidateSourceLines = acceptedSourceLines ++ submissionLines
      candidateSource = unlines candidateSourceLines
      formatSuccess newSourceLines allOutputs =
        let deltaOutputs =
              case stripPrefix acceptedOutputs allOutputs of
                Just suffix -> suffix
                Nothing -> drop (length acceptedOutputs) allOutputs
         in Right VmSubmissionResult {vmResultLines = newSourceLines, vmResultOutputs = allOutputs, vmResultDeltaOutputs = deltaOutputs}
   in case submissionLines of
        [singleLine]
          | isDisplayedExpressionLine singleLine ->
              let fallbackSourceLines = acceptedSourceLines ++ ["print __python_hs_repl_repr__(" ++ singleLine ++ ")"]
                  fallbackSource = unlines fallbackSourceLines
               in case runSourceVm fallbackSource of
                    Right allOutputs -> formatSuccess fallbackSourceLines allOutputs
                    Left _ -> runCandidate
        _ -> runCandidate
  where
    acceptedSourceLines = processVmSubmissionAcceptedSourceLines config
    acceptedOutputs = processVmSubmissionAcceptedOutputs config
    submissionLines = processVmSubmissionSubmissionLines config
    candidateSourceLines = acceptedSourceLines ++ submissionLines

    runCandidate =
      case runSourceVm (unlines candidateSourceLines) of
        Right allOutputs -> formatSuccess candidateSourceLines allOutputs
        Left err -> Left err

    formatSuccess newSourceLines allOutputs =
      let deltaOutputs =
            case stripPrefix acceptedOutputs allOutputs of
              Just suffix -> suffix
              Nothing -> drop (length acceptedOutputs) allOutputs
       in Right VmSubmissionResult {vmResultLines = newSourceLines, vmResultOutputs = allOutputs, vmResultDeltaOutputs = deltaOutputs}

    isDisplayedExpressionLine line =
      case scanTokens (line ++ "\n") of
        Left _ -> False
        Right tokens ->
          case parseExpr tokens of
            Right ParseResult {parseResultValue = expr, parseResultRemaining = remaining} ->
              onlyLineEnd remaining && isDisplayedExpression expr
            Left _ -> False

    isDisplayedExpression expr =
      case expr of
        CallExpr {} -> False
        CallValueExpr {} -> False
        IdentifierExpr {identifierExprName = identifierName} -> not (elem '.' identifierName)
        _ -> True

    onlyLineEnd [Token NewlineToken _ _, Token EOFToken _ _] = True
    onlyLineEnd [Token EOFToken _ _] = True
    onlyLineEnd _ = False
