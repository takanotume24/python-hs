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
      maybeElse = compileTryExceptMaybeElse config
      exceptClauses = compileTryExceptExceptClauses config
      maybeFinally = compileTryExceptMaybeFinally config
      hasElse = case maybeElse of
        Just _ -> True
        Nothing -> False
      compileOptionalSuite index maybeSuite =
        case maybeSuite of
          Nothing -> Right (CompileExprResult {compileExprResultCode = [], compileExprResultEndIndex = index})
          Just suiteStmts -> compileStatements index inFunction maybeLoop suiteStmts
   in case maybeFinally of
        Nothing -> do
          tryResult <- compileStatements (baseIndex + 1) inFunction maybeLoop tryStmts
          let tryEnd = compileExprResultEndIndex tryResult
          elseResult <- compileOptionalSuite (tryEnd + 2) maybeElse
          let elseEnd = compileExprResultEndIndex elseResult
              dispatchStart = case maybeElse of
                Just _ -> elseEnd + 1
                Nothing -> elseEnd
          dispatchResult <- compileExceptDispatch dispatchStart exceptClauses
          let finalEnd = compileExprResultEndIndex dispatchResult
              successJump =
                if hasElse
                  then [Jump {jumpTarget = tryEnd + 2}]
                  else [Jump {jumpTarget = finalEnd}]
              code =
                [PushExceptionHandler {pushExceptionHandlerIp = dispatchStart}]
                  ++ compileExprResultCode tryResult
                  ++ [PopExceptionHandler]
                  ++ successJump
                  ++ (if hasElse then compileExprResultCode elseResult ++ [Jump {jumpTarget = finalEnd}] else [])
                  ++ compileExprResultCode dispatchResult
          pure (CompileExprResult {compileExprResultCode = code, compileExprResultEndIndex = finalEnd})
        Just finallyStmts -> do
          tryResult <- compileStatements (baseIndex + 2) inFunction maybeLoop tryStmts
          let tryEnd = compileExprResultEndIndex tryResult
          elseResult <- compileOptionalSuite (tryEnd + 2) maybeElse
          let elseEnd = compileExprResultEndIndex elseResult
              dispatchStart = case maybeElse of
                Just _ -> elseEnd + 1
                Nothing -> elseEnd
          dispatchResult <- compileExceptDispatch dispatchStart exceptClauses
          let finallyStartIndex = compileExprResultEndIndex dispatchResult + 1
          finallyResult <- compileStatements (finallyStartIndex + 1) inFunction maybeLoop finallyStmts
          let successJump =
                if hasElse
                  then [Jump {jumpTarget = tryEnd + 2}]
                  else [Jump {jumpTarget = finallyStartIndex}]
              code =
                [PushFinallyHandler {pushFinallyHandlerIp = finallyStartIndex}, PushExceptionHandler {pushExceptionHandlerIp = dispatchStart}]
                  ++ compileExprResultCode tryResult
                  ++ [PopExceptionHandler]
                  ++ successJump
                  ++ (if hasElse then compileExprResultCode elseResult ++ [Jump {jumpTarget = finallyStartIndex}] else [])
                  ++ compileExprResultCode dispatchResult
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
