module PythonHS.VM.CompileExprAt (compileExprAt) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AndOperator, OrOperator))
import PythonHS.AST.Expr (Expr (BinaryExpr, CallExpr, CallValueExpr, DictExpr, FloatExpr, IdentifierExpr, IntegerExpr, LambdaDefaultsExpr, LambdaExpr, ListComprehensionClausesExpr, ListComprehensionExpr, ListExpr, NoneExpr, NotExpr, StringExpr, UnaryMinusExpr, WalrusExpr), Expr)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (FloatValue, IntValue, NoneValue, StringValue))
import PythonHS.VM.CompileCallArgsAt (compileCallArgsAt)
import PythonHS.VM.CompileComprehensionClauses (compileComprehensionClauses)
import PythonHS.VM.CompileDefaults (compileDefaults)
import PythonHS.VM.CompileDictEntriesAt (compileDictEntriesAt)
import PythonHS.VM.CompileExprItemsAt (compileExprItemsAt)
import PythonHS.VM.CompileLogicalExpr (compileLogicalExpr)
import PythonHS.VM.ExprPosition (exprPosition)
import PythonHS.VM.Instruction (Instruction (ApplyBinary, ApplyNot, ApplyUnaryMinus, BuildDict, BuildList, BuildListComprehension, CallFunction, CallValueFunction, CreateLambda, DupTop, LoadName, PushConst, ReturnTop, StoreName), Instruction)

compileExprAt :: Int -> Expr -> Either String ([Instruction], Int)
compileExprAt baseIndex expr =
  case expr of
    IntegerExpr n _ -> Right ([PushConst (IntValue n)], baseIndex + 1)
    FloatExpr n _ -> Right ([PushConst (FloatValue n)], baseIndex + 1)
    StringExpr s _ -> Right ([PushConst (StringValue s)], baseIndex + 1)
    NoneExpr _ -> Right ([PushConst NoneValue], baseIndex + 1)
    ListExpr elements _ -> do
      (elementCode, elementEnd) <- compileExprItemsAt compileExprAt baseIndex elements
      pure (elementCode ++ [BuildList (length elements)], elementEnd + 1)
    ListComprehensionExpr valueExpr loopName iterExpr pos -> do
      (iterCode, _) <- compileExprAt 0 iterExpr
      (valueCode, _) <- compileExprAt 0 valueExpr
      let clauses = [([loopName], iterCode ++ [ReturnTop], [])]
      pure ([BuildListComprehension clauses (valueCode ++ [ReturnTop]) pos], baseIndex + 1)
    ListComprehensionClausesExpr valueExpr clausesExpr pos -> do
      clauses <- compileComprehensionClauses compileExprAt clausesExpr
      (valueCode, _) <- compileExprAt 0 valueExpr
      pure ([BuildListComprehension clauses (valueCode ++ [ReturnTop]) pos], baseIndex + 1)
    DictExpr entries _ -> do
      (entryCode, entryEnd) <- compileDictEntriesAt compileExprAt baseIndex entries
      pure (entryCode ++ [BuildDict (length entries)], entryEnd + 1)
    IdentifierExpr name pos -> Right ([LoadName name pos], baseIndex + 1)
    UnaryMinusExpr unaryExpr pos -> do
      (exprCode, exprEnd) <- compileExprAt baseIndex unaryExpr
      pure (exprCode ++ [ApplyUnaryMinus pos], exprEnd + 1)
    NotExpr notExpr pos -> do
      (exprCode, exprEnd) <- compileExprAt baseIndex notExpr
      pure (exprCode ++ [ApplyNot pos], exprEnd + 1)
    WalrusExpr name valueExpr _ -> do
      (valueCode, valueEnd) <- compileExprAt baseIndex valueExpr
      pure (valueCode ++ [DupTop, StoreName name], valueEnd + 2)
    LambdaExpr params bodyExpr pos -> do
      (bodyCode, _) <- compileExprAt 0 bodyExpr
      let lambdaName = "__lambda_" ++ showPos pos
      pure ([CreateLambda lambdaName params [] (bodyCode ++ [ReturnTop])], baseIndex + 1)
    LambdaDefaultsExpr params defaults bodyExpr pos -> do
      (defaultCodes, _) <- compileDefaults compileExprAt defaults
      (bodyCode, _) <- compileExprAt 0 bodyExpr
      let lambdaName = "__lambda_" ++ showPos pos
      pure ([CreateLambda lambdaName params defaultCodes (bodyCode ++ [ReturnTop])], baseIndex + 1)
    BinaryExpr AndOperator left right _ -> compileLogicalExpr compileExprAt AndOperator baseIndex left right
    BinaryExpr OrOperator left right _ -> compileLogicalExpr compileExprAt OrOperator baseIndex left right
    BinaryExpr op left right pos -> do
      (leftCode, leftEnd) <- compileExprAt baseIndex left
      (rightCode, rightEnd) <- compileExprAt leftEnd right
      pure (leftCode ++ rightCode ++ [ApplyBinary op pos], rightEnd + 1)
    CallExpr fname args pos -> do
      compiledArgs <- compileCallArgsAt compileExprAt args
      pure ([CallFunction fname compiledArgs pos], baseIndex + 1)
    CallValueExpr callee args pos -> do
      (calleeCode, calleeEnd) <- compileExprAt baseIndex callee
      compiledArgs <- compileCallArgsAt compileExprAt args
      pure (calleeCode ++ [CallValueFunction compiledArgs pos], calleeEnd + 1)
    _ -> Left ("VM compile error: unsupported expression at " ++ showPos (exprPosition expr))
