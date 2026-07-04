module PythonHS.VM.CompileExprAt (compileExprAt) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AndOperator, OrOperator))
import PythonHS.AST.Expr (Expr (BinaryExpr, CallExpr, CallValueExpr, DictExpr, FloatExpr, IdentifierExpr, IndexExpr, IntegerExpr, LambdaDefaultsExpr, LambdaExpr, ListComprehensionClausesExpr, ListComprehensionExpr, ListExpr, NoneExpr, NotExpr, SliceExpr, StringExpr, TupleExpr, UnaryMinusExpr, WalrusExpr))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (FloatValue, IntValue, NoneValue, StringValue))
import PythonHS.VM.CompileCallArgsAt (compileCallArgsAt)
import PythonHS.VM.CompileComprehensionClauses (compileComprehensionClauses)
import PythonHS.VM.CompileDefaults (compileDefaults)
import PythonHS.VM.CompileDictEntriesAt (compileDictEntriesAt)
import PythonHS.VM.CompileExprItemsAt (compileExprItemsAt)
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.CompileLogicalExpr (compileLogicalExpr)
import PythonHS.VM.ExprPosition (exprPosition)
import PythonHS.VM.Instruction (Instruction (ApplyBinary, ApplyNot, ApplyUnaryMinus, BuildDict, BuildList, BuildListComprehension, BuildTuple, CallFunction, CallValueFunction, CreateLambda, DupTop, LoadName, PushConst, ReturnTop, StoreName))

compileExprAt :: Int -> Expr -> Either String CompileExprResult
compileExprAt baseIndex expr =
  case expr of
    IntegerExpr n _ -> Right (CompileExprResult [PushConst (IntValue n)] (baseIndex + 1))
    FloatExpr n _ -> Right (CompileExprResult [PushConst (FloatValue n)] (baseIndex + 1))
    StringExpr s _ -> Right (CompileExprResult [PushConst (StringValue s)] (baseIndex + 1))
    NoneExpr _ -> Right (CompileExprResult [PushConst NoneValue] (baseIndex + 1))
    ListExpr elements _ -> do
      itemsResult <- compileExprItemsAt compileExprAt baseIndex elements
      pure (CompileExprResult (compileExprResultCode itemsResult ++ [BuildList (length elements)]) (compileExprResultEndIndex itemsResult + 1))
    TupleExpr elements _ -> do
      itemsResult <- compileExprItemsAt compileExprAt baseIndex elements
      pure (CompileExprResult (compileExprResultCode itemsResult ++ [BuildTuple (length elements)]) (compileExprResultEndIndex itemsResult + 1))
    ListComprehensionExpr valueExpr loopName iterExpr pos -> do
      iterResult <- compileExprAt 0 iterExpr
      valueResult <- compileExprAt 0 valueExpr
      let clauses = [([loopName], compileExprResultCode iterResult ++ [ReturnTop], [])]
      pure (CompileExprResult [BuildListComprehension clauses (compileExprResultCode valueResult ++ [ReturnTop]) pos] (baseIndex + 1))
    ListComprehensionClausesExpr valueExpr clausesExpr pos -> do
      clauses <- compileComprehensionClauses compileExprAt clausesExpr
      valueResult <- compileExprAt 0 valueExpr
      pure (CompileExprResult [BuildListComprehension clauses (compileExprResultCode valueResult ++ [ReturnTop]) pos] (baseIndex + 1))
    DictExpr entries _ -> do
      let compileForDict b e = fmap (\r -> (compileExprResultCode r, compileExprResultEndIndex r)) (compileExprAt b e)
      (entryCode, entryEnd) <- compileDictEntriesAt compileForDict baseIndex entries
      pure (CompileExprResult (entryCode ++ [BuildDict (length entries)]) (entryEnd + 1))
    IdentifierExpr name pos -> Right (CompileExprResult [LoadName name pos] (baseIndex + 1))
    UnaryMinusExpr unaryExpr pos -> do
      unaryResult <- compileExprAt baseIndex unaryExpr
      pure (CompileExprResult (compileExprResultCode unaryResult ++ [ApplyUnaryMinus pos]) (compileExprResultEndIndex unaryResult + 1))
    NotExpr notExpr pos -> do
      notResult <- compileExprAt baseIndex notExpr
      pure (CompileExprResult (compileExprResultCode notResult ++ [ApplyNot pos]) (compileExprResultEndIndex notResult + 1))
    WalrusExpr name valueExpr _ -> do
      valueResult <- compileExprAt baseIndex valueExpr
      pure (CompileExprResult (compileExprResultCode valueResult ++ [DupTop, StoreName name]) (compileExprResultEndIndex valueResult + 2))
    LambdaExpr params bodyExpr pos -> do
      bodyResult <- compileExprAt 0 bodyExpr
      let lambdaName = "__lambda_" ++ showPos pos
      pure (CompileExprResult [CreateLambda lambdaName params [] (compileExprResultCode bodyResult ++ [ReturnTop])] (baseIndex + 1))
    LambdaDefaultsExpr params defaults bodyExpr pos -> do
      (defaultCodes, _) <- compileDefaults compileExprAt defaults
      bodyResult <- compileExprAt 0 bodyExpr
      let lambdaName = "__lambda_" ++ showPos pos
      pure (CompileExprResult [CreateLambda lambdaName params defaultCodes (compileExprResultCode bodyResult ++ [ReturnTop])] (baseIndex + 1))
    BinaryExpr AndOperator left right _ -> compileLogicalExpr compileExprAt AndOperator baseIndex left right
    BinaryExpr OrOperator left right _ -> compileLogicalExpr compileExprAt OrOperator baseIndex left right
    BinaryExpr op left right pos -> do
      leftResult <- compileExprAt baseIndex left
      rightResult <- compileExprAt (compileExprResultEndIndex leftResult) right
      pure (CompileExprResult (compileExprResultCode leftResult ++ compileExprResultCode rightResult ++ [ApplyBinary op pos]) (compileExprResultEndIndex rightResult + 1))
    CallExpr fname args pos -> do
      let compileForCall b e = fmap (\r -> (compileExprResultCode r, compileExprResultEndIndex r)) (compileExprAt b e)
      compiledArgs <- compileCallArgsAt compileForCall args
      pure (CompileExprResult [CallFunction fname compiledArgs pos] (baseIndex + 1))
    CallValueExpr callee args pos -> do
      calleeResult <- compileExprAt baseIndex callee
      let compileForCall b e = fmap (\r -> (compileExprResultCode r, compileExprResultEndIndex r)) (compileExprAt b e)
      compiledArgs <- compileCallArgsAt compileForCall args
      pure (CompileExprResult (compileExprResultCode calleeResult ++ [CallValueFunction compiledArgs pos]) (compileExprResultEndIndex calleeResult + 1))
    IndexExpr containerExpr indexExpr pos -> do
      containerResult <- compileExprAt 0 containerExpr
      indexResult <- compileExprAt 0 indexExpr
      let callArgs = [(compileExprResultCode containerResult, Nothing, pos), (compileExprResultCode indexResult, Nothing, pos)]
      pure (CompileExprResult [CallFunction "__python_hs_getitem__" callArgs pos] (baseIndex + 1))
    SliceExpr containerExpr maybeStart maybeEnd pos -> do
      containerResult <- compileExprAt 0 containerExpr
      let defaultNone = [PushConst NoneValue]
      startResult <-
        case maybeStart of
          Just startExpr -> compileExprAt 0 startExpr
          Nothing -> Right (CompileExprResult defaultNone 1)
      endResult <-
        case maybeEnd of
          Just endExpr -> compileExprAt 0 endExpr
          Nothing -> Right (CompileExprResult defaultNone 1)
      let callArgs = [(compileExprResultCode containerResult, Nothing, pos), (compileExprResultCode startResult, Nothing, pos), (compileExprResultCode endResult, Nothing, pos)]
      pure (CompileExprResult [CallFunction "__python_hs_slice__" callArgs pos] (baseIndex + 1))
    _ -> Left ("VM compile error: unsupported expression at " ++ showPos (exprPosition expr))
