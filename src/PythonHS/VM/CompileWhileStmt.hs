module PythonHS.VM.CompileWhileStmt (compileWhileStmt) where

import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.CompileWhileStmtConfig (CompileWhileStmtConfig (..))
import PythonHS.VM.Instruction (Instruction (..))
import PythonHS.VM.LoopContext (LoopContext (..))

compileWhileStmt :: CompileWhileStmtConfig -> Either String CompileExprResult
compileWhileStmt config = do
  let compileExprAt' = compileWhileStmtCompileExpr config
      compileStatements = compileWhileStmtCompileStatements config
      baseIndex = compileWhileStmtBaseIndex config
      inFunction = compileWhileStmtInFunction config
      body = compileWhileStmtBody config
      elseBody = maybe [] id (compileWhileStmtMaybeElse config)
      whilePos = compileWhileStmtPos config
      loopStartIndex = baseIndex + 1
  condResult <- compileExprAt' loopStartIndex (compileWhileStmtCond config)
  let jumpIfFalseIndex = compileExprResultEndIndex condResult
      bodyStartIndex = jumpIfFalseIndex + 1
      guardIndex = bodyStartIndex
      firstBodyStmtIndex = guardIndex + 1
      provisionalLoopContext = Just LoopContext {loopContextBreakTarget = 0, loopContextContinueTarget = loopStartIndex}
  provisionalBodyResult <- compileStatements firstBodyStmtIndex inFunction provisionalLoopContext body
  let elseStartIndex = compileExprResultEndIndex provisionalBodyResult + 1
  provisionalElseResult <- compileStatements elseStartIndex inFunction Nothing elseBody
  let loopEndIndex = compileExprResultEndIndex provisionalElseResult
      breakTarget = loopEndIndex + 1
      loopContext = Just LoopContext {loopContextBreakTarget = breakTarget, loopContextContinueTarget = loopStartIndex}
  bodyResult <- compileStatements firstBodyStmtIndex inFunction loopContext body
  elseResult <- compileStatements elseStartIndex inFunction Nothing elseBody
  let code = [PushLoopBoundary] ++ compileExprResultCode condResult ++ [JumpIfFalse {jumpIfFalseTarget = elseStartIndex}, LoopGuard {loopGuardPos = whilePos}] ++ compileExprResultCode bodyResult ++ [Jump {jumpTarget = loopStartIndex}] ++ compileExprResultCode elseResult ++ [PopLoopBoundary]
  pure (CompileExprResult {compileExprResultCode = code, compileExprResultEndIndex = breakTarget})
