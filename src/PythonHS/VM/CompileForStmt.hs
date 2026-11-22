module PythonHS.VM.CompileForStmt (compileForStmt) where

import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.CompileForStmtConfig (CompileForStmtConfig (..))
import PythonHS.VM.ExprPosition (exprPosition)
import PythonHS.VM.Instruction (Instruction (..))
import PythonHS.VM.LoopContext (LoopContext (..))

compileForStmt :: CompileForStmtConfig -> Either String CompileExprResult
compileForStmt config = do
  let compileExprAt' = compileForStmtCompileExpr config
      compileStatements = compileForStmtCompileStatements config
      baseIndex = compileForStmtBaseIndex config
      inFunction = compileForStmtInFunction config
      iterExpr = compileForStmtIter config
      body = compileForStmtBody config
      elseBody = maybe [] id (compileForStmtMaybeElse config)
      forPos = compileForStmtPos config
      loopStartIndex = baseIndex + 1
  iterResult <- compileExprAt' loopStartIndex iterExpr
  let setupIndex = compileExprResultEndIndex iterResult
      nextIndex = setupIndex + 1
      guardIndex = nextIndex + 1
      bodyStartIndex = guardIndex + 1
      provisionalLoopContext = Just LoopContext {loopContextBreakTarget = 0, loopContextContinueTarget = nextIndex}
  provisionalBodyResult <- compileStatements bodyStartIndex inFunction provisionalLoopContext body
  let elseStartIndex = compileExprResultEndIndex provisionalBodyResult + 1
  provisionalElseResult <- compileStatements elseStartIndex inFunction Nothing elseBody
  let loopEndIndex = compileExprResultEndIndex provisionalElseResult
      breakTarget = loopEndIndex + 1
      loopContext = Just LoopContext {loopContextBreakTarget = breakTarget, loopContextContinueTarget = nextIndex}
  bodyResult <- compileStatements bodyStartIndex inFunction loopContext body
  elseResult <- compileStatements elseStartIndex inFunction Nothing elseBody
  let iterPos = exprPosition iterExpr
      code = [PushLoopBoundary] ++ compileExprResultCode iterResult ++ [ForSetup {forSetupNextIndex = nextIndex, forSetupPos = iterPos}, ForNext {forNextName = compileForStmtName config, forNextLoopEndIndex = elseStartIndex, forNextPos = iterPos}, LoopGuard {loopGuardPos = forPos}] ++ compileExprResultCode bodyResult ++ [Jump {jumpTarget = nextIndex}] ++ compileExprResultCode elseResult ++ [PopLoopBoundary]
  pure (CompileExprResult {compileExprResultCode = code, compileExprResultEndIndex = breakTarget})
