module PythonHS.VM.TakeFinallyHandler (takeFinallyHandler) where

import PythonHS.VM.DecodeFinallyHandler (decodeFinallyHandler)
import PythonHS.VM.IsFinallyHandler (isFinallyHandler)
import PythonHS.VM.LoopBoundaryMarker (loopBoundaryMarker)

-- | Finds the nearest finally handler, returning the handlers that remain
-- after removing everything down to and including it. When the first flag is
-- true the search stops at the loop boundary marker.
takeFinallyHandler :: Bool -> [Int] -> Maybe ([Int], Int)
takeFinallyHandler stopAtLoopBoundary handlers =
  case handlers of
    [] -> Nothing
    handlerIp : restHandlers
      | stopAtLoopBoundary && handlerIp == loopBoundaryMarker -> Nothing
      | isFinallyHandler handlerIp -> Just (restHandlers, decodeFinallyHandler handlerIp)
      | otherwise -> takeFinallyHandler stopAtLoopBoundary restHandlers
