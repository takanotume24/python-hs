module PythonHS.VM.CompileWithStmt (compileWithStmt) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Stmt (Stmt)
import PythonHS.AST.WithContext (ContextManager (..), WithEntry (..), WithExit (..))
import PythonHS.Evaluator.Value (Value (NoneValue))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprAt (compileExprAt)
import PythonHS.VM.CompileExprAtConfig (CompileExprAtConfig (..))
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.CompileWithStmtConfig (CompileWithStmtConfig (..))
import PythonHS.VM.Instruction (Instruction (..))

compileWithStmt :: CompileWithStmtConfig -> Either String CompileExprResult
compileWithStmt config = do
  let baseIndex = compileWithStmtBaseIndex config
      inFunction = compileWithStmtInFunction config
      maybeLoop = compileWithStmtMaybeLoop config
      cmExpr = compileWithStmtCmExpr config
      maybeVarName = compileWithStmtMaybeVarName config
      body = compileWithStmtBody config
      withPos = compileWithStmtWithPos config
      compileStatementsFn = compileWithStmtCompileStatements config
      ctxManager = ContextManager {contextManagerExpr = cmExpr, contextManagerVarName = maybeVarName, contextManagerPos = withPos}
  contextManagerResult <- compileExprAt' baseIndex (contextManagerExpr ctxManager)
  let contextManagerCode = compileExprResultCode contextManagerResult
  let contextManagerVar = "__context_manager_" ++ show baseIndex ++ "__"
  let setupCode = contextManagerCode ++ [StoreName {storeNameName = contextManagerVar}]

  let entryInstruction = CallFunction {callFunctionName = "__enter__", callFunctionArgs = [([LoadName {loadNameName = contextManagerVar, loadNamePos = contextManagerPos ctxManager}], Nothing, contextManagerPos ctxManager)], callFunctionPos = contextManagerPos ctxManager}
  let withEntry = WithEntry {entryCallExpr = CallExpr {callExprName = "__enter__", callExprArgs = [contextManagerExpr ctxManager], callExprPos = contextManagerPos ctxManager}, entryCallInstruction = entryInstruction, entryPos = contextManagerPos ctxManager}
  let enterCode = [LoadName {loadNameName = contextManagerVar, loadNamePos = contextManagerPos ctxManager}, entryCallInstruction withEntry]

  let storeCode = case contextManagerVarName ctxManager of
        Just varName -> [StoreName {storeNameName = varName}]
        Nothing -> []
  let setupEndIndex = baseIndex + length setupCode + length enterCode + length storeCode
  let bodyStartIndex = setupEndIndex + 1
  bodyResult <- compileStatementsFn bodyStartIndex inFunction maybeLoop body
  let bodyCode = compileExprResultCode bodyResult
  let bodyEndIndex = compileExprResultEndIndex bodyResult
  let exitNormalStartIndex = bodyEndIndex + 2
  let nonePos = ([PushConst {pushConstValue = NoneValue}], Nothing, contextManagerPos ctxManager)

  let exitNormalInstruction =
        CallFunction
          { callFunctionName = "__exit__",
            callFunctionArgs =
              [ ([LoadName {loadNameName = contextManagerVar, loadNamePos = contextManagerPos ctxManager}], Nothing, contextManagerPos ctxManager),
                nonePos,
                nonePos,
                nonePos
              ],
            callFunctionPos = contextManagerPos ctxManager
          }
  let exitNormal = WithExit {exitCallExpr = CallExpr {callExprName = "__exit__", callExprArgs = [contextManagerExpr ctxManager, NoneExpr {noneExprPos = contextManagerPos ctxManager}, NoneExpr {noneExprPos = contextManagerPos ctxManager}, NoneExpr {noneExprPos = contextManagerPos ctxManager}], callExprPos = contextManagerPos ctxManager}, exitCallInstruction = exitNormalInstruction, exitPos = contextManagerPos ctxManager, exitIsException = False}
  let exitNormalCode = [LoadName {loadNameName = contextManagerVar, loadNamePos = contextManagerPos ctxManager}, exitCallInstruction exitNormal]

  let exitExceptionStartIndex = exitNormalStartIndex + length exitNormalCode + 1

  let exitExceptionInstruction =
        CallFunction
          { callFunctionName = "__exit__",
            callFunctionArgs =
              [ ([LoadName {loadNameName = contextManagerVar, loadNamePos = contextManagerPos ctxManager}], Nothing, contextManagerPos ctxManager),
                ([LoadPendingException], Nothing, contextManagerPos ctxManager),
                ([LoadPendingException], Nothing, contextManagerPos ctxManager),
                nonePos
              ],
            callFunctionPos = contextManagerPos ctxManager
          }
  let exitException = WithExit {exitCallExpr = CallExpr {callExprName = "__exit__", callExprArgs = [contextManagerExpr ctxManager, StringExpr {stringExprValue = "Exception", stringExprPos = contextManagerPos ctxManager}, StringExpr {stringExprValue = "error", stringExprPos = contextManagerPos ctxManager}, NoneExpr {noneExprPos = contextManagerPos ctxManager}], callExprPos = contextManagerPos ctxManager}, exitCallInstruction = exitExceptionInstruction, exitPos = contextManagerPos ctxManager, exitIsException = True}
  let exitExceptionCode =
        [ LoadName {loadNameName = contextManagerVar, loadNamePos = contextManagerPos ctxManager},
          exitCallInstruction exitException,
          CheckWithResult
        ]
  let nextIndex = exitExceptionStartIndex + length exitExceptionCode
  let allCode =
        setupCode
          ++ enterCode
          ++ storeCode
          ++ [PushWithHandler {pushWithHandlerIp = exitExceptionStartIndex}]
          ++ bodyCode
          ++ [PopExceptionHandler, Jump {jumpTarget = exitNormalStartIndex}]
          ++ exitNormalCode
          ++ [Jump {jumpTarget = nextIndex}]
          ++ exitExceptionCode
  pure (CompileExprResult {compileExprResultCode = allCode, compileExprResultEndIndex = nextIndex})
  where
    compileExprAt' b e = compileExprAt CompileExprAtConfig {compileExprAtBaseIndex = b, compileExprAtExpr = e}
