module PythonHS.VM.EncodeFinallyHandler (encodeFinallyHandler) where

encodeFinallyHandler :: Int -> Int
encodeFinallyHandler finallyStartIp = negate (finallyStartIp + 2)
