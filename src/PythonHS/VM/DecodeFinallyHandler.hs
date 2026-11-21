module PythonHS.VM.DecodeFinallyHandler (decodeFinallyHandler) where

decodeFinallyHandler :: Int -> Int
decodeFinallyHandler encodedHandler = negate encodedHandler - 2
