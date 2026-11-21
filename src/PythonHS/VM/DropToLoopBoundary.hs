module PythonHS.VM.DropToLoopBoundary (dropToLoopBoundary) where

import PythonHS.VM.LoopBoundaryMarker (loopBoundaryMarker)

-- | Drops handlers down to the loop boundary. When the flag is true the
-- boundary marker itself is dropped as well.
dropToLoopBoundary :: Bool -> [Int] -> [Int]
dropToLoopBoundary includeMarker handlers =
  case handlers of
    [] -> []
    handlerIp : restHandlers
      | handlerIp == loopBoundaryMarker ->
          if includeMarker then restHandlers else handlerIp : restHandlers
      | otherwise -> dropToLoopBoundary includeMarker restHandlers
