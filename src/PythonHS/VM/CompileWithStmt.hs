module PythonHS.VM.CompileWithStmt (compileWithStmt) where

import PythonHS.AST.Expr (Expr (CallExpr, NoneExpr, StringExpr))
import PythonHS.AST.Stmt (Stmt)
import PythonHS.AST.WithContext (ContextManager (..), WithEntry (..), WithExit (..))
import PythonHS.Evaluator.Value (Value (NoneValue))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprAt (compileExprAt)
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.Instruction (Instruction (CallFunction, CheckWithResult, Jump, LoadName, LoadPendingException, PopExceptionHandler, PushConst, PushWithHandler, StoreName))

compileWithStmt ::
  Int ->
  Bool ->
  Maybe (Int, Int) ->
  Expr ->
  Maybe String ->
  [Stmt] ->
  Position ->
  (Int -> Bool -> Maybe (Int, Int) -> [Stmt] -> Either String CompileExprResult) ->
  Either String CompileExprResult
compileWithStmt baseIndex inFunction maybeLoop cmExpr maybeVarName body withPos compileStatementsFn = do
  let ctxManager = ContextManager cmExpr maybeVarName withPos
  contextManagerResult <- compileExprAt baseIndex (contextManagerExpr ctxManager)
  let contextManagerCode = compileExprResultCode contextManagerResult
  let contextManagerVar = "__context_manager_" ++ show baseIndex ++ "__"
  let setupCode = contextManagerCode ++ [StoreName contextManagerVar]

  let entryInstruction = CallFunction "__enter__" [([LoadName contextManagerVar (contextManagerPos ctxManager)], Nothing, contextManagerPos ctxManager)] (contextManagerPos ctxManager)
  let withEntry = WithEntry (CallExpr "__enter__" [contextManagerExpr ctxManager] (contextManagerPos ctxManager)) entryInstruction (contextManagerPos ctxManager)
  let enterCode = [LoadName contextManagerVar (contextManagerPos ctxManager), entryCallInstruction withEntry]

  let storeCode = case contextManagerVarName ctxManager of
        Just varName -> [StoreName varName]
        Nothing -> []
  let setupEndIndex = baseIndex + length setupCode + length enterCode + length storeCode
  let bodyStartIndex = setupEndIndex + 1
  bodyResult <- compileStatementsFn bodyStartIndex inFunction maybeLoop body
  let bodyCode = compileExprResultCode bodyResult
  let bodyEndIndex = compileExprResultEndIndex bodyResult
  let exitNormalStartIndex = bodyEndIndex + 2
  let nonePos = ([PushConst NoneValue], Nothing, contextManagerPos ctxManager)

  let exitNormalInstruction =
        CallFunction
          "__exit__"
          [ ([LoadName contextManagerVar (contextManagerPos ctxManager)], Nothing, contextManagerPos ctxManager),
            nonePos,
            nonePos,
            nonePos
          ]
          (contextManagerPos ctxManager)
  let exitNormal = WithExit (CallExpr "__exit__" [contextManagerExpr ctxManager, NoneExpr (contextManagerPos ctxManager), NoneExpr (contextManagerPos ctxManager), NoneExpr (contextManagerPos ctxManager)] (contextManagerPos ctxManager)) exitNormalInstruction (contextManagerPos ctxManager) False
  let exitNormalCode = [LoadName contextManagerVar (contextManagerPos ctxManager), exitCallInstruction exitNormal]

  let exitExceptionStartIndex = exitNormalStartIndex + length exitNormalCode + 1

  let exitExceptionInstruction =
        CallFunction
          "__exit__"
          [ ([LoadName contextManagerVar (contextManagerPos ctxManager)], Nothing, contextManagerPos ctxManager),
            ([LoadPendingException], Nothing, contextManagerPos ctxManager),
            ([LoadPendingException], Nothing, contextManagerPos ctxManager),
            nonePos
          ]
          (contextManagerPos ctxManager)
  let exitException = WithExit (CallExpr "__exit__" [contextManagerExpr ctxManager, StringExpr "Exception" (contextManagerPos ctxManager), StringExpr "error" (contextManagerPos ctxManager), NoneExpr (contextManagerPos ctxManager)] (contextManagerPos ctxManager)) exitExceptionInstruction (contextManagerPos ctxManager) True
  let exitExceptionCode =
        [ LoadName contextManagerVar (contextManagerPos ctxManager),
          exitCallInstruction exitException,
          CheckWithResult
        ]
  let nextIndex = exitExceptionStartIndex + length exitExceptionCode
  let allCode =
        setupCode
          ++ enterCode
          ++ storeCode
          ++ [PushWithHandler exitExceptionStartIndex]
          ++ bodyCode
          ++ [PopExceptionHandler, Jump exitNormalStartIndex]
          ++ exitNormalCode
          ++ [Jump nextIndex]
          ++ exitExceptionCode
  pure (CompileExprResult allCode nextIndex)
