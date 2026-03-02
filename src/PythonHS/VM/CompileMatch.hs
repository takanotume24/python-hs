module PythonHS.VM.CompileMatch (compileMatch) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Pattern (Pattern)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.Instruction (Instruction (Jump, JumpIfFalse, LoadName, MatchPattern, StoreName), Instruction)

compileMatch ::
  (Int -> Expr -> Either String ([Instruction], Int)) ->
  (Int -> Bool -> Maybe (Int, Int) -> [Stmt] -> Either String ([Instruction], Int)) ->
  Int ->
  Bool ->
  Maybe (Int, Int) ->
  Expr ->
  [(Pattern, Maybe Expr, [Stmt], Position)] ->
  Either String ([Instruction], Int)
compileMatch compileExprAt compileStatements baseIndex inFunction maybeLoop subjectExpr cases = do
  (subjectCode, subjectEnd) <- compileExprAt baseIndex subjectExpr
  let subjectName = "__python_hs_match_subject_" ++ show baseIndex
  (caseCode, endIndex) <- compileCases (subjectEnd + 1) subjectName cases
  let fullCode = subjectCode ++ [StoreName subjectName] ++ patchEndJumps endIndex caseCode
  pure (fullCode, endIndex)
  where
    compileCases nextIndex _ [] = Right ([], nextIndex)
    compileCases nextIndex subjectName ((pat, maybeGuard, bodyStmts, patPos) : restCases) = do
      let loadAndMatch = [LoadName subjectName patPos, MatchPattern pat patPos]
      let jumpAfterMatchIndex = nextIndex + 2
      (guardCode, guardEnd) <-
        case maybeGuard of
          Nothing -> Right ([], jumpAfterMatchIndex + 1)
          Just guardExpr -> do
            (compiledGuard, guardExprEnd) <- compileExprAt (jumpAfterMatchIndex + 1) guardExpr
            Right (compiledGuard ++ [JumpIfFalse 0], guardExprEnd + 1)
      (bodyCode, bodyEnd) <- compileStatements guardEnd inFunction maybeLoop bodyStmts
      let nextCaseStart = bodyEnd + 1
      let guardCodeFixed = patchGuardJump nextCaseStart guardCode
      (restCode, endIndex) <- compileCases nextCaseStart subjectName restCases
      let thisCase =
            loadAndMatch
              ++ [JumpIfFalse nextCaseStart]
              ++ guardCodeFixed
              ++ bodyCode
              ++ [Jump 0]
      Right (thisCase ++ restCode, endIndex)

    patchGuardJump target codes =
      case reverse codes of
        JumpIfFalse _ : restRev -> reverse (JumpIfFalse target : restRev)
        _ -> codes

    patchEndJumps target = map (\instr -> case instr of Jump 0 -> Jump target; _ -> instr)
