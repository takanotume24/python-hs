module PythonHS.VM.CompileTryExcept (compileTryExcept) where

import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.Instruction (Instruction (Jump, JumpIfFalse, LoadPendingException, MatchExceptionType, PopExceptionHandler, PushExceptionHandler, PushFinallyHandler, RaisePendingError, RaisePendingException, StoreName), Instruction)

compileTryExcept ::
  (Int -> Bool -> Maybe (Int, Int) -> [Stmt] -> Either String CompileExprResult) ->
  Int ->
  Bool ->
  Maybe (Int, Int) ->
  [Stmt] ->
  [(Maybe String, Maybe String, [Stmt], Position)] ->
  Maybe [Stmt] ->
  Either String CompileExprResult
compileTryExcept compileStatements baseIndex inFunction maybeLoop tryStmts exceptClauses maybeFinally =
  case maybeFinally of
    Nothing -> do
      let tryStartIndex = baseIndex + 1
      tryResult <- compileStatements tryStartIndex inFunction maybeLoop tryStmts
      let exceptStartIndex = compileExprResultEndIndex tryResult + 2
      exceptResult <- compileExceptDispatch exceptStartIndex exceptClauses
      let code =
            [PushExceptionHandler exceptStartIndex]
              ++ compileExprResultCode tryResult
              ++ [PopExceptionHandler, Jump (compileExprResultEndIndex exceptResult)]
              ++ compileExprResultCode exceptResult
      pure (CompileExprResult code (compileExprResultEndIndex exceptResult))
    Just finallyStmts -> do
      let tryStartIndex = baseIndex + 2
      tryResult <- compileStatements tryStartIndex inFunction maybeLoop tryStmts
      let exceptStartIndex = compileExprResultEndIndex tryResult + 2
      exceptResult <- compileExceptDispatch exceptStartIndex exceptClauses
      let finallyStartIndex = compileExprResultEndIndex exceptResult + 1
      let finallyBodyStartIndex = finallyStartIndex + 1
      finallyResult <- compileStatements finallyBodyStartIndex inFunction maybeLoop finallyStmts
      let code =
            [PushFinallyHandler finallyStartIndex, PushExceptionHandler exceptStartIndex]
              ++ compileExprResultCode tryResult
              ++ [PopExceptionHandler, Jump finallyStartIndex]
              ++ compileExprResultCode exceptResult
              ++ [Jump finallyStartIndex, PopExceptionHandler]
              ++ compileExprResultCode finallyResult
              ++ [RaisePendingError]
      pure (CompileExprResult code (compileExprResultEndIndex finallyResult + 1))
  where
    compileExceptDispatch dispatchStart clauses = do
      dispatchResult <- compileExceptClauses dispatchStart clauses
      let rethrowCode = [RaisePendingException]
      pure (CompileExprResult (compileExprResultCode dispatchResult ++ rethrowCode) (compileExprResultEndIndex dispatchResult + 1))

    compileExceptClauses currentIndex clauses =
      case clauses of
        [] -> Right (CompileExprResult [] currentIndex)
        (maybeTypeName, maybeAliasName, exceptStmts, _) : restClauses -> do
          let aliasCode =
                case maybeAliasName of
                  Nothing -> []
                  Just aliasName -> [LoadPendingException, StoreName aliasName]
          let bodyStartIndex = currentIndex + 2 + length aliasCode
          bodyResult <- compileStatements bodyStartIndex inFunction maybeLoop exceptStmts
          restResult <- compileExceptClauses (compileExprResultEndIndex bodyResult + 1) restClauses
          let nextClauseStart = compileExprResultEndIndex bodyResult + 1
          let clauseCode =
                [MatchExceptionType maybeTypeName]
                  ++ [JumpIfFalse nextClauseStart]
                  ++ aliasCode
                  ++ compileExprResultCode bodyResult
                  ++ [Jump (compileExprResultEndIndex restResult + 1)]
          pure (CompileExprResult (clauseCode ++ compileExprResultCode restResult) (compileExprResultEndIndex restResult))
