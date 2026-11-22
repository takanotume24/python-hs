module PythonHS.VM.CompileDeleteSubscript (compileDeleteSubscript) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (NoneValue))
import PythonHS.VM.CompileDeleteSubscriptConfig (CompileDeleteSubscriptConfig (..))
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.CompiledCallArg (CompiledCallArg (..))
import PythonHS.VM.Instruction (Instruction (..))

compileDeleteSubscript :: CompileDeleteSubscriptConfig -> Either String CompileExprResult
compileDeleteSubscript config =
  case target of
    IndexExpr {indexExprBase = baseExpr, indexExprIndex = indexExpr} -> do
      rootName <- baseName baseExpr
      baseResult <- compileExpr 0 baseExpr
      indexResult <- compileExpr 0 indexExpr
      pure (storeResult rootName "__python_hs_delitem__" [baseResult, indexResult])
    SliceExpr {sliceExprBase = baseExpr, sliceExprStart = maybeStart, sliceExprEnd = maybeEnd} -> do
      rootName <- baseName baseExpr
      baseResult <- compileExpr 0 baseExpr
      startResult <- compileOptional maybeStart
      endResult <- compileOptional maybeEnd
      pure (storeResult rootName "__python_hs_delslice__" [baseResult, startResult, endResult])
    _ -> Left ("VM compile error: unsupported del target at " ++ showPos pos)
  where
    compileExpr = compileDeleteSubscriptCompileExpr config
    baseIndex = compileDeleteSubscriptBaseIndex config
    target = compileDeleteSubscriptTarget config
    pos = compileDeleteSubscriptPos config

    baseName expr =
      case expr of
        IdentifierExpr {identifierExprName = name} -> Right name
        _ -> Left ("VM compile error: unsupported del target at " ++ showPos pos)

    compileOptional maybeExpr =
      case maybeExpr of
        Nothing -> Right (CompileExprResult {compileExprResultCode = [PushConst {pushConstValue = NoneValue}], compileExprResultEndIndex = baseIndex + 1})
        Just expr -> compileExpr 0 expr

    storeResult rootName functionName argResults =
      let callArgs = fmap toArg argResults
          code =
            [ CallFunction
                { callFunctionName = functionName,
                  callFunctionArgs = callArgs,
                  callFunctionPos = pos
                },
              StoreName {storeNameName = rootName}
            ]
       in CompileExprResult {compileExprResultCode = code, compileExprResultEndIndex = baseIndex + 2}

    toArg result =
      CompiledCallArg
        { compiledCallArgCode = compileExprResultCode result,
          compiledCallArgKeyword = Nothing,
          compiledCallArgPos = pos
        }
