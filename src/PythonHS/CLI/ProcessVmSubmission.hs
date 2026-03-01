module PythonHS.CLI.ProcessVmSubmission (processVmSubmission) where

import Data.List (stripPrefix)
import PythonHS.Lexer.ScanTokens (scanTokens)
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (EOFToken, NewlineToken))
import PythonHS.Parser.ParseExpr (parseExpr)
import PythonHS.RunSourceVm (runSourceVm)

processVmSubmission :: [String] -> [String] -> [String] -> Either String ([String], [String], [String])
processVmSubmission acceptedSourceLines acceptedOutputs submissionLines =
  let candidateSourceLines = acceptedSourceLines ++ submissionLines
      candidateSource = unlines candidateSourceLines
      formatSuccess newSourceLines allOutputs =
        let deltaOutputs =
              case stripPrefix acceptedOutputs allOutputs of
                Just suffix -> suffix
                Nothing -> drop (length acceptedOutputs) allOutputs
         in Right (newSourceLines, allOutputs, deltaOutputs)
   in case runSourceVm candidateSource of
        Right allOutputs -> formatSuccess candidateSourceLines allOutputs
        Left originalErr ->
          case submissionLines of
            [singleLine] ->
              if isExpressionLine singleLine
                then
                  let fallbackSourceLines = acceptedSourceLines ++ ["print __python_hs_repl_repr__(" ++ singleLine ++ ")"]
                      fallbackSource = unlines fallbackSourceLines
                   in case runSourceVm fallbackSource of
                        Right allOutputs -> formatSuccess fallbackSourceLines allOutputs
                        Left _ -> Left originalErr
                else Left originalErr
            _ -> Left originalErr
  where
    isExpressionLine line =
      case scanTokens (line ++ "\n") of
        Left _ -> False
        Right tokens ->
          case parseExpr tokens of
            Right (_, remaining) -> onlyLineEnd remaining
            Left _ -> False

    onlyLineEnd [Token NewlineToken _ _, Token EOFToken _ _] = True
    onlyLineEnd [Token EOFToken _ _] = True
    onlyLineEnd _ = False