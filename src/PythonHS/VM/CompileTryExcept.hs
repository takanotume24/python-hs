module PythonHS.VM.CompileTryExcept (compileTryExcept) where

import PythonHS.AST.ExceptSuite (ExceptSuite (..))
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.CompileTryExceptConfig (CompileTryExceptConfig (..))
import PythonHS.VM.Instruction (Instruction (..))

compileTryExcept :: CompileTryExceptConfig -> Either String CompileExprResult
compileTryExcept config =
  let compileStatements = compileTryExceptCompileStatements config
      baseIndex = compileTryExceptBaseIndex config
      inFunction = compileTryExceptInFunction config
      maybeLoop = compileTryExceptMaybeLoop config
      tryStmts = compileTryExceptTryStmts config
      exceptClauses = compileTryExceptExceptClauses config
      maybeFinally = compileTryExceptMaybeFinally config
   in case maybeFinally of
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
        ExceptSuite {exceptSuiteType = maybeTypeNames, exceptSuiteAlias = maybeAliasName, exceptSuiteBody = exceptStmts} : restClauses -> do
          let aliasCode =
                case maybeAliasName of
                  Nothing -> []
                  Just aliasName -> [LoadPendingException, StoreName {storeNameName = aliasName}]
          let deleteAliasCode =
                case maybeAliasName of
                  Nothing -> []
                  Just aliasName -> [DeleteName {deleteNameTarget = aliasName}]
          let bodyStartIndex = currentIndex + 2 + length aliasCode
          let compileStatements = compileTryExceptCompileStatements config
              inFunction = compileTryExceptInFunction config
              maybeLoop = compileTryExceptMaybeLoop config
          bodyResult <- compileStatements bodyStartIndex inFunction maybeLoop exceptStmts
          let nextClauseStart = compileExprResultEndIndex bodyResult + length deleteAliasCode + 1
          restResult <- compileExceptClauses nextClauseStart restClauses
          let clauseCode =
                [MatchExceptionType {matchExceptionTypeName = maybeTypeNames}]
                  ++ [JumpIfFalse {jumpIfFalseTarget = nextClauseStart}]
                  ++ aliasCode
                  ++ compileExprResultCode bodyResult
                  ++ deleteAliasCode
                  ++ [Jump {jumpTarget = compileExprResultEndIndex restResult + 1}]
          pure (CompileExprResult {compileExprResultCode = clauseCode ++ compileExprResultCode restResult, compileExprResultEndIndex = compileExprResultEndIndex restResult})
