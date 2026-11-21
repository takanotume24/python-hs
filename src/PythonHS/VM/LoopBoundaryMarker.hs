module PythonHS.VM.LoopBoundaryMarker (loopBoundaryMarker) where

-- | Sentinel pushed onto the exception handler stack when entering a loop.
-- It lets break/continue find the finally blocks that belong to the loop.
loopBoundaryMarker :: Int
loopBoundaryMarker = -1
