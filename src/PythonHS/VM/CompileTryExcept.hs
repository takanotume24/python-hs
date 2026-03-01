module PythonHS.VM.CompileTryExcept (compileTryExcept) where

import PythonHS.AST.Stmt (Stmt)
import PythonHS.VM.Instruction (Instruction (Jump, PopExceptionHandler, PushExceptionHandler, PushFinallyHandler, RaisePendingError), Instruction)

compileTryExcept ::
  (Int -> Bool -> Maybe (Int, Int) -> [Stmt] -> Either String ([Instruction], Int)) ->
  Int ->
  Bool ->
  Maybe (Int, Int) ->
  [Stmt] ->
  [[Stmt]] ->
  Maybe [Stmt] ->
  Either String ([Instruction], Int)
compileTryExcept compileStatements baseIndex inFunction maybeLoop tryStmts exceptSuites maybeFinally =
  let exceptStmts = firstExceptSuite exceptSuites
   in case maybeFinally of
        Nothing -> do
          let tryStartIndex = baseIndex + 1
          (tryCode, tryEndIndex) <- compileStatements tryStartIndex inFunction maybeLoop tryStmts
          let popHandlerIndex = tryEndIndex
          let jumpAfterTryIndex = popHandlerIndex + 1
          let exceptStartIndex = jumpAfterTryIndex + 1
          (exceptCode, exceptEndIndex) <- compileStatements exceptStartIndex inFunction maybeLoop exceptStmts
          let code =
                [PushExceptionHandler exceptStartIndex]
                  ++ tryCode
                  ++ [PopExceptionHandler, Jump exceptEndIndex]
                  ++ exceptCode
          pure (code, exceptEndIndex)
        Just finallyStmts -> do
          let tryStartIndex = baseIndex + 2
          (tryCode, tryEndIndex) <- compileStatements tryStartIndex inFunction maybeLoop tryStmts
          let popTryHandlerIndex = tryEndIndex
          let jumpAfterTryIndex = popTryHandlerIndex + 1
          let exceptStartIndex = jumpAfterTryIndex + 1
          (exceptCode, exceptEndIndex) <- compileStatements exceptStartIndex inFunction maybeLoop exceptStmts
          let jumpToFinallyAfterExceptIndex = exceptEndIndex
          let finallyStartIndex = jumpToFinallyAfterExceptIndex + 1
          let popFinallyHandlerIndex = finallyStartIndex
          let finallyBodyStartIndex = popFinallyHandlerIndex + 1
          (finallyCode, finallyBodyEndIndex) <- compileStatements finallyBodyStartIndex inFunction maybeLoop finallyStmts
          let raisePendingIndex = finallyBodyEndIndex
          let code =
                [PushFinallyHandler finallyStartIndex, PushExceptionHandler exceptStartIndex]
                  ++ tryCode
                  ++ [PopExceptionHandler, Jump finallyStartIndex]
                  ++ exceptCode
                  ++ [Jump finallyStartIndex, PopExceptionHandler]
                  ++ finallyCode
                  ++ [RaisePendingError]
          pure (code, raisePendingIndex + 1)
  where
    firstExceptSuite suites =
      case suites of
        firstSuite : _ -> firstSuite
        [] -> []
