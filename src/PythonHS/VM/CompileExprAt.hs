module PythonHS.VM.CompileExprAt (compileExprAt) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AndOperator, OrOperator))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CompileCallArgsAt (compileCallArgsAt)
import PythonHS.VM.CompileCallArgsAtConfig (CompileCallArgsAtConfig (..))
import PythonHS.VM.CompileComprehensionClauses (compileComprehensionClauses)
import PythonHS.VM.CompileComprehensionClausesConfig (CompileComprehensionClausesConfig (..))
import PythonHS.VM.CompileDefaults (compileDefaults)
import PythonHS.VM.CompileDefaultsConfig (CompileDefaultsConfig (..))
import PythonHS.VM.CompileDictEntriesAt (compileDictEntriesAt)
import PythonHS.VM.CompileDictEntriesAtConfig (CompileDictEntriesAtConfig (..))
import PythonHS.VM.CompileExprAtConfig (CompileExprAtConfig (..))
import PythonHS.VM.CompileExprItemsAt (compileExprItemsAt)
import PythonHS.VM.CompileExprItemsAtConfig (CompileExprItemsAtConfig (..))
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.CompileLogicalExpr (compileLogicalExpr)
import PythonHS.VM.CompileLogicalExprConfig (CompileLogicalExprConfig (..))
import PythonHS.VM.ExprPosition (exprPosition)
import PythonHS.VM.Instruction (Instruction (..))

compileExprAt :: CompileExprAtConfig -> Either String CompileExprResult
compileExprAt config =
  let baseIndex = compileExprAtBaseIndex config
      expr = compileExprAtExpr config
      compileExprAt' b e = compileExprAt CompileExprAtConfig {compileExprAtBaseIndex = b, compileExprAtExpr = e}
   in case expr of
        IntegerExpr {integerExprValue = n} -> Right (CompileExprResult {compileExprResultCode = [PushConst {pushConstValue = IntValue {intValue = n}}], compileExprResultEndIndex = baseIndex + 1})
        FloatExpr {floatExprValue = n} -> Right (CompileExprResult {compileExprResultCode = [PushConst {pushConstValue = FloatValue {floatValue = n}}], compileExprResultEndIndex = baseIndex + 1})
        StringExpr {stringExprValue = s} -> Right (CompileExprResult {compileExprResultCode = [PushConst {pushConstValue = StringValue {stringValue = s}}], compileExprResultEndIndex = baseIndex + 1})
        NoneExpr {} -> Right (CompileExprResult {compileExprResultCode = [PushConst {pushConstValue = NoneValue}], compileExprResultEndIndex = baseIndex + 1})
        ListExpr {listExprItems = elements} -> do
          itemsResult <- compileExprItemsAt CompileExprItemsAtConfig {compileExprItemsAtCompileExpr = compileExprAt', compileExprItemsAtBaseIndex = baseIndex, compileExprItemsAtExprs = elements}
          pure (CompileExprResult {compileExprResultCode = compileExprResultCode itemsResult ++ [BuildList {buildListCount = length elements}], compileExprResultEndIndex = compileExprResultEndIndex itemsResult + 1})
        TupleExpr {tupleExprItems = elements} -> do
          itemsResult <- compileExprItemsAt CompileExprItemsAtConfig {compileExprItemsAtCompileExpr = compileExprAt', compileExprItemsAtBaseIndex = baseIndex, compileExprItemsAtExprs = elements}
          pure (CompileExprResult {compileExprResultCode = compileExprResultCode itemsResult ++ [BuildTuple {buildTupleCount = length elements}], compileExprResultEndIndex = compileExprResultEndIndex itemsResult + 1})
        ListComprehensionExpr {listComprehensionExprValue = valueExpr, listComprehensionExprLoopName = loopName, listComprehensionExprIter = iterExpr, listComprehensionExprPos = pos} -> do
          iterResult <- compileExprAt' 0 iterExpr
          valueResult <- compileExprAt' 0 valueExpr
          let clauses = [([loopName], compileExprResultCode iterResult ++ [ReturnTop], [])]
          pure (CompileExprResult {compileExprResultCode = [BuildListComprehension {buildListComprehensionClauses = clauses, buildListComprehensionValueCode = compileExprResultCode valueResult ++ [ReturnTop], buildListComprehensionPos = pos}], compileExprResultEndIndex = baseIndex + 1})
        ListComprehensionClausesExpr {listComprehensionClausesExprValue = valueExpr, listComprehensionClausesExprClauses = clausesExpr, listComprehensionClausesExprPos = pos} -> do
          clauses <- compileComprehensionClauses CompileComprehensionClausesConfig {compileComprehensionClausesCompileExpr = compileExprAt', compileComprehensionClausesClauses = clausesExpr}
          valueResult <- compileExprAt' 0 valueExpr
          pure (CompileExprResult {compileExprResultCode = [BuildListComprehension {buildListComprehensionClauses = clauses, buildListComprehensionValueCode = compileExprResultCode valueResult ++ [ReturnTop], buildListComprehensionPos = pos}], compileExprResultEndIndex = baseIndex + 1})
        DictExpr {dictExprEntries = entries} -> do
          let compileForDict b e = fmap (\r -> (compileExprResultCode r, compileExprResultEndIndex r)) (compileExprAt' b e)
          (entryCode, entryEnd) <- compileDictEntriesAt CompileDictEntriesAtConfig {compileDictEntriesAtCompileExpr = compileForDict, compileDictEntriesAtBaseIndex = baseIndex, compileDictEntriesAtEntries = entries}
          pure (CompileExprResult {compileExprResultCode = entryCode ++ [BuildDict {buildDictCount = length entries}], compileExprResultEndIndex = entryEnd + 1})
        IdentifierExpr {identifierExprName = name, identifierExprPos = pos} -> Right (CompileExprResult {compileExprResultCode = [LoadName {loadNameName = name, loadNamePos = pos}], compileExprResultEndIndex = baseIndex + 1})
        UnaryMinusExpr {unaryMinusExprValue = unaryExpr, unaryMinusExprPos = pos} -> do
          unaryResult <- compileExprAt' baseIndex unaryExpr
          pure (CompileExprResult {compileExprResultCode = compileExprResultCode unaryResult ++ [ApplyUnaryMinus {applyUnaryMinusPos = pos}], compileExprResultEndIndex = compileExprResultEndIndex unaryResult + 1})
        NotExpr {notExprValue = notExpr, notExprPos = pos} -> do
          notResult <- compileExprAt' baseIndex notExpr
          pure (CompileExprResult {compileExprResultCode = compileExprResultCode notResult ++ [ApplyNot {applyNotPos = pos}], compileExprResultEndIndex = compileExprResultEndIndex notResult + 1})
        WalrusExpr {walrusExprName = name, walrusExprValue = valueExpr} -> do
          valueResult <- compileExprAt' baseIndex valueExpr
          pure (CompileExprResult {compileExprResultCode = compileExprResultCode valueResult ++ [DupTop, StoreName {storeNameName = name}], compileExprResultEndIndex = compileExprResultEndIndex valueResult + 2})
        LambdaExpr {lambdaExprParams = params, lambdaExprValue = bodyExpr, lambdaExprPos = pos} -> do
          bodyResult <- compileExprAt' 0 bodyExpr
          let lambdaName = "__lambda_" ++ showPos pos
          pure (CompileExprResult {compileExprResultCode = [CreateLambda {createLambdaName = lambdaName, createLambdaParams = params, createLambdaDefaultCodes = [], createLambdaCode = compileExprResultCode bodyResult ++ [ReturnTop]}], compileExprResultEndIndex = baseIndex + 1})
        LambdaDefaultsExpr {lambdaDefaultsExprParams = params, lambdaDefaultsExprDefaults = defaults, lambdaDefaultsExprValue = bodyExpr, lambdaDefaultsExprPos = pos} -> do
          (defaultCodes, _) <- compileDefaults CompileDefaultsConfig {compileDefaultsCompileExpr = compileExprAt', compileDefaultsDefaults = defaults}
          bodyResult <- compileExprAt' 0 bodyExpr
          let lambdaName = "__lambda_" ++ showPos pos
          pure (CompileExprResult {compileExprResultCode = [CreateLambda {createLambdaName = lambdaName, createLambdaParams = params, createLambdaDefaultCodes = defaultCodes, createLambdaCode = compileExprResultCode bodyResult ++ [ReturnTop]}], compileExprResultEndIndex = baseIndex + 1})
        BinaryExpr {binaryExprOp = AndOperator, binaryExprLeft = left, binaryExprRight = right} -> compileLogicalExpr CompileLogicalExprConfig {compileLogicalExprCompileExpr = compileExprAt', compileLogicalExprOp = AndOperator, compileLogicalExprBaseIndex = baseIndex, compileLogicalExprLeft = left, compileLogicalExprRight = right}
        BinaryExpr {binaryExprOp = OrOperator, binaryExprLeft = left, binaryExprRight = right} -> compileLogicalExpr CompileLogicalExprConfig {compileLogicalExprCompileExpr = compileExprAt', compileLogicalExprOp = OrOperator, compileLogicalExprBaseIndex = baseIndex, compileLogicalExprLeft = left, compileLogicalExprRight = right}
        BinaryExpr {binaryExprOp = op, binaryExprLeft = left, binaryExprRight = right, binaryExprPos = pos} -> do
          leftResult <- compileExprAt' baseIndex left
          rightResult <- compileExprAt' (compileExprResultEndIndex leftResult) right
          pure (CompileExprResult {compileExprResultCode = compileExprResultCode leftResult ++ compileExprResultCode rightResult ++ [ApplyBinary {applyBinaryOp = op, applyBinaryPos = pos}], compileExprResultEndIndex = compileExprResultEndIndex rightResult + 1})
        CallExpr {callExprName = fname, callExprArgs = args, callExprPos = pos} -> do
          let compileForCall b e = fmap (\r -> (compileExprResultCode r, compileExprResultEndIndex r)) (compileExprAt' b e)
          compiledArgs <- compileCallArgsAt CompileCallArgsAtConfig {compileCallArgsAtCompileExpr = compileForCall, compileCallArgsAtArgs = args}
          pure (CompileExprResult {compileExprResultCode = [CallFunction {callFunctionName = fname, callFunctionArgs = compiledArgs, callFunctionPos = pos}], compileExprResultEndIndex = baseIndex + 1})
        CallValueExpr {callValueExprCallee = callee, callValueExprArgs = args, callValueExprPos = pos} -> do
          calleeResult <- compileExprAt' baseIndex callee
          let compileForCall b e = fmap (\r -> (compileExprResultCode r, compileExprResultEndIndex r)) (compileExprAt' b e)
          compiledArgs <- compileCallArgsAt CompileCallArgsAtConfig {compileCallArgsAtCompileExpr = compileForCall, compileCallArgsAtArgs = args}
          pure (CompileExprResult {compileExprResultCode = compileExprResultCode calleeResult ++ [CallValueFunction {callValueFunctionArgs = compiledArgs, callValueFunctionPos = pos}], compileExprResultEndIndex = compileExprResultEndIndex calleeResult + 1})
        IndexExpr {indexExprBase = containerExpr, indexExprIndex = indexExpr, indexExprPos = pos} -> do
          containerResult <- compileExprAt' 0 containerExpr
          indexResult <- compileExprAt' 0 indexExpr
          let callArgs = [(compileExprResultCode containerResult, Nothing, pos), (compileExprResultCode indexResult, Nothing, pos)]
          pure (CompileExprResult {compileExprResultCode = [CallFunction {callFunctionName = "__python_hs_getitem__", callFunctionArgs = callArgs, callFunctionPos = pos}], compileExprResultEndIndex = baseIndex + 1})
        SliceExpr {sliceExprBase = containerExpr, sliceExprStart = maybeStart, sliceExprEnd = maybeEnd, sliceExprPos = pos} -> do
          containerResult <- compileExprAt' 0 containerExpr
          let defaultNone = [PushConst {pushConstValue = NoneValue}]
          startResult <- case maybeStart of
            Just startExpr -> compileExprAt' 0 startExpr
            Nothing -> Right (CompileExprResult {compileExprResultCode = defaultNone, compileExprResultEndIndex = 1})
          endResult <- case maybeEnd of
            Just endExpr -> compileExprAt' 0 endExpr
            Nothing -> Right (CompileExprResult {compileExprResultCode = defaultNone, compileExprResultEndIndex = 1})
          let callArgs = [(compileExprResultCode containerResult, Nothing, pos), (compileExprResultCode startResult, Nothing, pos), (compileExprResultCode endResult, Nothing, pos)]
          pure (CompileExprResult {compileExprResultCode = [CallFunction {callFunctionName = "__python_hs_slice__", callFunctionArgs = callArgs, callFunctionPos = pos}], compileExprResultEndIndex = baseIndex + 1})
        _ -> Left ("VM compile error: unsupported expression at " ++ showPos (exprPosition expr))
