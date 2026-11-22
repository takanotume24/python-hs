module Test.VM.RunSourceVmPart34Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 34" $ do
  describe "text file I/O" $ do
    it "writes and reads a file" $ withSystemTempDirectory "python-hs-io" $ \dir -> do
      let path = dir </> "sample.txt"
      runSourceVm
        ( unlines
            [ "f = open('" ++ path ++ "', 'w')",
              "f.write('hello')",
              "f.close()",
              "g = open('" ++ path ++ "')",
              "print g.read()",
              "g.close()"
            ]
        )
        `shouldBe` Right ["hello"]

    it "appends to a file" $ withSystemTempDirectory "python-hs-io" $ \dir -> do
      let path = dir </> "append.txt"
      runSourceVm
        ( unlines
            [ "f = open('" ++ path ++ "', 'w')",
              "f.write('a')",
              "f.close()",
              "f = open('" ++ path ++ "', 'a')",
              "f.write('b')",
              "f.close()",
              "g = open('" ++ path ++ "')",
              "print g.read()",
              "g.close()"
            ]
        )
        `shouldBe` Right ["ab"]

    it "closes a file through the with statement" $ withSystemTempDirectory "python-hs-io" $ \dir -> do
      let path = dir </> "with.txt"
      runSourceVm
        ( unlines
            [ "with open('" ++ path ++ "', 'w') as f:",
              "    f.write('hi')",
              "with open('" ++ path ++ "') as g:",
              "    print g.read()"
            ]
        )
        `shouldBe` Right ["hi"]

  describe "file errors" $ do
    it "rejects reading from a write-only file" $ withSystemTempDirectory "python-hs-io" $ \dir -> do
      let path = dir </> "write-only.txt"
      let source = "f = open('" ++ path ++ "', 'w')\nprint f.read()\n"
      case runSourceVm source of
        Left message -> message `shouldBe` "Runtime error: file not open for reading at 2:9"
        Right outputs -> fail ("unexpected success: " ++ show outputs)

    it "rejects writing to a read-only file" $ withSystemTempDirectory "python-hs-io" $ \dir -> do
      let path = dir </> "read-only.txt"
      writeFile path ""
      let source = "f = open('" ++ path ++ "')\nf.write('x')\n"
      case runSourceVm source of
        Left message -> message `shouldBe` "Runtime error: file not open for writing at 2:3"
        Right outputs -> fail ("unexpected success: " ++ show outputs)

    it "rejects reading a closed file" $ withSystemTempDirectory "python-hs-io" $ \dir -> do
      let path = dir </> "closed.txt"
      writeFile path "data"
      let source = "f = open('" ++ path ++ "')\nf.close()\nprint f.read()\n"
      case runSourceVm source of
        Left message -> message `shouldBe` "Runtime error: I/O operation on closed file at 3:9"
        Right outputs -> fail ("unexpected success: " ++ show outputs)

    it "rejects an unsupported mode" $ withSystemTempDirectory "python-hs-io" $ \dir -> do
      let path = dir </> "mode.txt"
      let source = "open('" ++ path ++ "', 'x')\n"
      case runSourceVm source of
        Left message -> message `shouldBe` "Value error: invalid mode 'x' at 1:1"
        Right outputs -> fail ("unexpected success: " ++ show outputs)

    it "rejects opening a missing file for reading" $ withSystemTempDirectory "python-hs-io" $ \dir -> do
      let path = dir </> "missing.txt"
      let source = "open('" ++ path ++ "')\n"
      case runSourceVm source of
        Left message -> message `shouldBe` ("Runtime error: file not found: " ++ path ++ " at 1:1")
        Right outputs -> fail ("unexpected success: " ++ show outputs)
