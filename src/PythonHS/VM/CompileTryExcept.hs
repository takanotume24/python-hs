module PythonHS.VM.CompileTryExcept (compileTryExcept) where

import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.Instruction (Instruction (..))

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
            [PushExceptionHandler {pushExceptionHandlerIp = exceptStartIndex}]
              ++ compileExprResultCode tryResult
              ++ [PopExceptionHandler, Jump {jumpTarget = compileExprResultEndIndex exceptResult}]
              ++ compileExprResultCode exceptResult
      pure (CompileExprResult {compileExprResultCode = code, compileExprResultEndIndex = compileExprResultEndIndex exceptResult})
    Just finallyStmts -> do
      let tryStartIndex = baseIndex + 2
      tryResult <- compileStatements tryStartIndex inFunction maybeLoop tryStmts
      let exceptStartIndex = compileExprResultEndIndex tryResult + 2
      exceptResult <- compileExceptDispatch exceptStartIndex exceptClauses
      let finallyStartIndex = compileExprResultEndIndex exceptResult + 1
      let finallyBodyStartIndex = finallyStartIndex + 1
      finallyResult <- compileStatements finallyBodyStartIndex inFunction maybeLoop finallyStmts
      let code =
            [PushFinallyHandler {pushFinallyHandlerIp = finallyStartIndex}, PushExceptionHandler {pushExceptionHandlerIp = exceptStartIndex}]
              ++ compileExprResultCode tryResult
              ++ [PopExceptionHandler, Jump {jumpTarget = finallyStartIndex}]
              ++ compileExprResultCode exceptResult
              ++ [Jump {jumpTarget = finallyStartIndex}, PopExceptionHandler]
              ++ compileExprResultCode finallyResult
              ++ [RaisePendingError]
      pure (CompileExprResult {compileExprResultCode = code, compileExprResultEndIndex = compileExprResultEndIndex finallyResult + 1})
  where
    compileExceptDispatch dispatchStart clauses = do
      dispatchResult <- compileExceptClauses dispatchStart clauses
      let rethrowCode = [RaisePendingException]
      pure (CompileExprResult {compileExprResultCode = compileExprResultCode dispatchResult ++ rethrowCode, compileExprResultEndIndex = compileExprResultEndIndex dispatchResult + 1})

    compileExceptClauses currentIndex clauses =
      case clauses of
        [] -> Right (CompileExprResult {compileExprResultCode = [], compileExprResultEndIndex = currentIndex})
        (maybeTypeName, maybeAliasName, exceptStmts, _) : restClauses -> do
          let aliasCode =
                case maybeAliasName of
                  Nothing -> []
                  Just aliasName -> [LoadPendingException, StoreName {storeNameName = aliasName}]
          let bodyStartIndex = currentIndex + 2 + length aliasCode
          bodyResult <- compileStatements bodyStartIndex inFunction maybeLoop exceptStmts
          restResult <- compileExceptClauses (compileExprResultEndIndex bodyResult + 1) restClauses
          let nextClauseStart = compileExprResultEndIndex bodyResult + 1
          let clauseCode =
                [MatchExceptionType {matchExceptionTypeName = maybeTypeName}]
                  ++ [JumpIfFalse {jumpIfFalseTarget = nextClauseStart}]
                  ++ aliasCode
                  ++ compileExprResultCode bodyResult
                  ++ [Jump {jumpTarget = compileExprResultEndIndex restResult + 1}]
          pure (CompileExprResult {compileExprResultCode = clauseCode ++ compileExprResultCode restResult, compileExprResultEndIndex = compileExprResultEndIndex restResult})
