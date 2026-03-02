module PythonHS.VM.ExecuteUnpackToNames (executeUnpackToNames) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.StoreNameWithAttr (storeNameWithAttr)
import PythonHS.VM.ToForIterable (toForIterable)

executeUnpackToNames ::
  Bool ->
  Set.Set String ->
  Position ->
  [String] ->
  Value ->
  Map.Map String Value ->
  Map.Map String Value ->
  Either String (Map.Map String Value, Map.Map String Value)
executeUnpackToNames isTopLevel globalDecls pos names value globalsNow localsNow = do
  unpackedValues <- toForIterable value pos
  if length unpackedValues /= length names
    then Left ("Value error: unpack mismatch at " ++ showPos pos)
    else storeUnpacked names unpackedValues globalsNow localsNow
  where
    storeUnpacked remainingNames remainingValues currentGlobals currentLocals =
      case (remainingNames, remainingValues) of
        ([], []) -> Right (currentGlobals, currentLocals)
        (name : restNames, nextValue : restValues) ->
          case storeNameWithAttr isTopLevel globalDecls name nextValue currentGlobals currentLocals of
            Left err -> Left err
            Right (nextGlobals, nextLocals) ->
              storeUnpacked restNames restValues nextGlobals nextLocals
        _ -> Left "VM runtime error: unpack internal mismatch"
