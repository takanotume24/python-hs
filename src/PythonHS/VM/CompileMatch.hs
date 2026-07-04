module PythonHS.VM.CompileMatch (compileMatch) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Pattern (Pattern)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.Instruction (Instruction (Jump, JumpIfFalse, LoadName, MatchPattern, StoreName))

compileMatch ::
  (Int -> Expr -> Either String CompileExprResult) ->
  (Int -> Bool -> Maybe (Int, Int) -> [Stmt] -> Either String CompileExprResult) ->
  Int ->
  Bool ->
  Maybe (Int, Int) ->
  Expr ->
  [(Pattern, Maybe Expr, [Stmt], Position)] ->
  Either String CompileExprResult
compileMatch compileExprAt compileStatements baseIndex inFunction maybeLoop subjectExpr cases = do
  subjectResult <- compileExprAt baseIndex subjectExpr
  let subjectName = "__python_hs_match_subject_" ++ show baseIndex
  (caseCode, endIndex) <- compileCases (compileExprResultEndIndex subjectResult + 1) subjectName cases
  let fullCode = compileExprResultCode subjectResult ++ [StoreName subjectName] ++ patchEndJumps endIndex caseCode
  pure (CompileExprResult fullCode endIndex)
  where
    compileCases nextIndex _ [] = Right ([], nextIndex)
    compileCases nextIndex subjectName ((pat, maybeGuard, bodyStmts, patPos) : restCases) = do
      let loadAndMatch = [LoadName subjectName patPos, MatchPattern pat patPos]
      let jumpAfterMatchIndex = nextIndex + 2
      guardResult <-
        case maybeGuard of
          Nothing -> Right (CompileExprResult [] (jumpAfterMatchIndex + 1))
          Just guardExpr -> do
            guardExprResult <- compileExprAt (jumpAfterMatchIndex + 1) guardExpr
            pure (CompileExprResult (compileExprResultCode guardExprResult ++ [JumpIfFalse 0]) (compileExprResultEndIndex guardExprResult + 1))
      bodyResult <- compileStatements (compileExprResultEndIndex guardResult) inFunction maybeLoop bodyStmts
      let nextCaseStart = compileExprResultEndIndex bodyResult + 1
      let guardCodeFixed = patchGuardJump nextCaseStart (compileExprResultCode guardResult)
      (restCode, endIndex) <- compileCases nextCaseStart subjectName restCases
      let thisCase =
            loadAndMatch
              ++ [JumpIfFalse nextCaseStart]
              ++ guardCodeFixed
              ++ compileExprResultCode bodyResult
              ++ [Jump 0]
      Right (thisCase ++ restCode, endIndex)

    patchGuardJump target codes =
      case reverse codes of
        JumpIfFalse _ : restRev -> reverse (JumpIfFalse target : restRev)
        _ -> codes

    patchEndJumps target = map (\instr -> case instr of Jump 0 -> Jump target; _ -> instr)
