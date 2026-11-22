module PythonHS.VM.CompileMatch (compileMatch) where

import PythonHS.AST.MatchCase (MatchCase (..))
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.CompileMatchConfig (CompileMatchConfig (..))
import PythonHS.VM.Instruction (Instruction (..))
import PythonHS.VM.MatchCasesCompileResult (MatchCasesCompileResult (..))

compileMatch :: CompileMatchConfig -> Either String CompileExprResult
compileMatch config = do
  subjectResult <- compileExpr baseIndex subjectExpr
  let subjectName = "__python_hs_match_subject_" ++ show baseIndex
  casesResult <- compileCases (compileExprResultEndIndex subjectResult + 1) subjectName cases
  let fullCode = compileExprResultCode subjectResult ++ [StoreName {storeNameName = subjectName}] ++ patchEndJumps (matchCasesCompileResultEndIndex casesResult) (matchCasesCompileResultCode casesResult)
  pure (CompileExprResult {compileExprResultCode = fullCode, compileExprResultEndIndex = matchCasesCompileResultEndIndex casesResult})
  where
    compileExpr = compileMatchCompileExpr config
    compileStatements = compileMatchCompileStatements config
    baseIndex = compileMatchBaseIndex config
    subjectExpr = compileMatchSubjectExpr config
    cases = compileMatchCases config
    inFunction = compileMatchInFunction config
    maybeLoop = compileMatchMaybeLoop config

    compileCases nextIndex _ [] = Right MatchCasesCompileResult {matchCasesCompileResultCode = [], matchCasesCompileResultEndIndex = nextIndex}
    compileCases nextIndex subjectName' (MatchCase {matchCasePattern = pat, matchCaseGuard = maybeGuard, matchCaseBody = bodyStmts, matchCasePos = patPos} : restCases) = do
      let loadAndMatch = [LoadName {loadNameName = subjectName', loadNamePos = patPos}, MatchPattern {matchPatternPattern = pat, matchPatternPos = patPos}]
      let jumpAfterMatchIndex = nextIndex + 2
      guardResult <-
        case maybeGuard of
          Nothing -> Right (CompileExprResult {compileExprResultCode = [], compileExprResultEndIndex = jumpAfterMatchIndex + 1})
          Just guardExpr -> do
            guardExprResult <- compileExpr (jumpAfterMatchIndex + 1) guardExpr
            pure (CompileExprResult {compileExprResultCode = compileExprResultCode guardExprResult ++ [JumpIfFalse {jumpIfFalseTarget = 0}], compileExprResultEndIndex = compileExprResultEndIndex guardExprResult + 1})
      bodyResult <- compileStatements (compileExprResultEndIndex guardResult) inFunction maybeLoop bodyStmts
      let nextCaseStart = compileExprResultEndIndex bodyResult + 1
      let guardCodeFixed = patchGuardJump nextCaseStart (compileExprResultCode guardResult)
      restResult <- compileCases nextCaseStart subjectName' restCases
      let thisCase =
            loadAndMatch
              ++ [JumpIfFalse {jumpIfFalseTarget = nextCaseStart}]
              ++ guardCodeFixed
              ++ compileExprResultCode bodyResult
              ++ [Jump {jumpTarget = 0}]
      Right MatchCasesCompileResult {matchCasesCompileResultCode = thisCase ++ matchCasesCompileResultCode restResult, matchCasesCompileResultEndIndex = matchCasesCompileResultEndIndex restResult}

    patchGuardJump target codes =
      case reverse codes of
        JumpIfFalse {jumpIfFalseTarget = 0} : restRev -> reverse (JumpIfFalse {jumpIfFalseTarget = target} : restRev)
        _ -> codes

    patchEndJumps target = map (\instr -> case instr of Jump {jumpTarget = 0} -> Jump {jumpTarget = target}; _ -> instr)
