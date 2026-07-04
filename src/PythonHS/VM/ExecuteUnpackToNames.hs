module PythonHS.VM.ExecuteUnpackToNames (executeUnpackToNames) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.StoreNameWithAttr (storeNameWithAttr)
import PythonHS.VM.ToForIterable (toForIterable)
import PythonHS.VM.ToForIterableConfig (ToForIterableConfig (..))
import PythonHS.VM.VMScopeContext (VMScopeContext)

executeUnpackToNames ::
  VMScopeContext ->
  Position ->
  [String] ->
  Value ->
  Map.Map String Value ->
  Map.Map String Value ->
  Either String (Map.Map String Value, Map.Map String Value)
executeUnpackToNames scopeCtx pos names value globalsNow localsNow = do
  unpackedValues <- toForIterable ToForIterableConfig {toForIterableValue = value, toForIterablePos = pos}
  if length unpackedValues /= length names
    then Left ("Value error: unpack mismatch at " ++ showPos pos)
    else storeUnpacked names unpackedValues globalsNow localsNow
  where
    storeUnpacked remainingNames remainingValues currentGlobals currentLocals =
      case (remainingNames, remainingValues) of
        ([], []) -> Right (currentGlobals, currentLocals)
        (name : restNames, nextValue : restValues) ->
          case storeNameWithAttr scopeCtx name nextValue currentGlobals currentLocals of
            Left err -> Left err
            Right (nextGlobals, nextLocals) ->
              storeUnpacked restNames restValues nextGlobals nextLocals
        _ -> Left "VM runtime error: unpack internal mismatch"
