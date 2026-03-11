module PythonHS.VM.CompileTryExcept (compileTryExcept) where

import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.Instruction (Instruction (Jump, JumpIfFalse, LoadPendingException, MatchExceptionType, PopExceptionHandler, PushExceptionHandler, PushFinallyHandler, RaisePendingError, RaisePendingException, StoreName), Instruction)

compileTryExcept ::
  (Int -> Bool -> Maybe (Int, Int) -> [Stmt] -> Either String ([Instruction], Int)) ->
  Int ->
  Bool ->
  Maybe (Int, Int) ->
  [Stmt] ->
  [(Maybe String, Maybe String, [Stmt], Position)] ->
  Maybe [Stmt] ->
  Either String ([Instruction], Int)
compileTryExcept compileStatements baseIndex inFunction maybeLoop tryStmts exceptClauses maybeFinally =
  case maybeFinally of
    Nothing -> do
      let tryStartIndex = baseIndex + 1
      (tryCode, tryEndIndex) <- compileStatements tryStartIndex inFunction maybeLoop tryStmts
      let exceptStartIndex = tryEndIndex + 2
      (exceptCode, exceptEndIndex) <- compileExceptDispatch exceptStartIndex exceptClauses
      let code =
            [PushExceptionHandler exceptStartIndex]
              ++ tryCode
              ++ [PopExceptionHandler, Jump exceptEndIndex]
              ++ exceptCode
      pure (code, exceptEndIndex)
    Just finallyStmts -> do
      let tryStartIndex = baseIndex + 2
      (tryCode, tryEndIndex) <- compileStatements tryStartIndex inFunction maybeLoop tryStmts
      let exceptStartIndex = tryEndIndex + 2
      (exceptCode, exceptEndIndex) <- compileExceptDispatch exceptStartIndex exceptClauses
      let finallyStartIndex = exceptEndIndex + 1
      let finallyBodyStartIndex = finallyStartIndex + 1
      (finallyCode, finallyBodyEndIndex) <- compileStatements finallyBodyStartIndex inFunction maybeLoop finallyStmts
      let code =
            [PushFinallyHandler finallyStartIndex, PushExceptionHandler exceptStartIndex]
              ++ tryCode
              ++ [PopExceptionHandler, Jump finallyStartIndex]
              ++ exceptCode
              ++ [Jump finallyStartIndex, PopExceptionHandler]
              ++ finallyCode
              ++ [RaisePendingError]
      pure (code, finallyBodyEndIndex + 1)
  where
    compileExceptDispatch dispatchStart clauses = do
      (dispatchCode, dispatchEnd) <- compileExceptClauses dispatchStart clauses
      let rethrowCode = [RaisePendingException]
      pure (dispatchCode ++ rethrowCode, dispatchEnd + 1)

    compileExceptClauses currentIndex clauses =
      case clauses of
        [] -> Right ([], currentIndex)
        (maybeTypeName, maybeAliasName, exceptStmts, _) : restClauses -> do
          let aliasCode =
                case maybeAliasName of
                  Nothing -> []
                  Just aliasName -> [LoadPendingException, StoreName aliasName]
          let bodyStartIndex = currentIndex + 2 + length aliasCode
          (bodyCode, bodyEndIndex) <- compileStatements bodyStartIndex inFunction maybeLoop exceptStmts
          (restCode, restEndIndex) <- compileExceptClauses (bodyEndIndex + 1) restClauses
          let nextClauseStart = bodyEndIndex + 1
          let clauseCode =
                [MatchExceptionType maybeTypeName]
                  ++ [JumpIfFalse nextClauseStart]
                  ++ aliasCode
                  ++ bodyCode
                  ++ [Jump (restEndIndex + 1)]
          pure (clauseCode ++ restCode, restEndIndex)
