module PythonHS.VM.IsFinallyHandler (isFinallyHandler) where

import PythonHS.VM.LoopBoundaryMarker (loopBoundaryMarker)

isFinallyHandler :: Int -> Bool
isFinallyHandler handlerIp = handlerIp < 0 && handlerIp /= loopBoundaryMarker
