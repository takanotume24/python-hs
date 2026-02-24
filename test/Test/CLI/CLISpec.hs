module Test.CLI.CLISpec (spec) where

import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStr, hClose)
import PythonHS.CLI (runFile, replEvalLines)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runFile / replEvalLines" $ do
  it "runs a file and returns printed output" $
    withSystemTempFile "script.pyhs" $ \path h -> do
      hPutStr h "x = 1\nprint x\n"
      hClose h
      res <- runFile path
      res `shouldBe` Right ["1"]

  it "runs an MVP scenario from file" $
    withSystemTempFile "mvp.pyhs" $ \path h -> do
      hPutStr h "x = 1\nif x:\nprint 10\nelse:\nprint 20\nwhile x < 3:\nx = x + 1\ndef id(v):\nreturn v\nprint id(x)\n"
      hClose h
      res <- runFile path
      res `shouldBe` Right ["10", "3"]

  it "returns an IO error for missing file" $ do
    res <- runFile "this-file-does-not-exist-xyz"
    case res of
      Left _ -> return ()
      Right _ -> error "expected Left for missing file"

  it "preserves environment across REPL inputs" $ do
    outs <- replEvalLines ["x = 1", "print x", ""]
    outs `shouldBe` ["1"]

  it "accepts multi-line blocks (function def) and preserves functions" $ do
    outs <- replEvalLines ["def add(a, b):", "print a", "", "print add(1, 2)", ""]
    outs `shouldBe` ["1", "0"]

  it "reports lexer/parse errors with prefix in REPL" $ do
    outs1 <- replEvalLines ["x @ 1", ""]
    outs1 `shouldBe` ["Error: UnexpectedCharacter '@'"]
    outs2 <- replEvalLines ["print", ""]
    outs2 `shouldBe` ["Error: ExpectedExpression (Position {line = 1, column = 6})"]

  it "handles if/else block and function return in REPL" $ do
    outs <- replEvalLines ["x = 0", "if x:", "print 1", "else:", "print 2", "", "def id(v):", "return v", "", "print id(5)", ""]
    outs `shouldBe` ["2", "5"]

  it "executes while loop block in REPL and keeps updated value" $ do
    outs <- replEvalLines ["x = 0", "while x < 3:", "x = x + 1", "", "print x", ""]
    outs `shouldBe` ["3"]