module Test.CLI.CLISpec (spec) where

import Control.Exception (bracket)
import Data.List (isInfixOf, isPrefixOf)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStr, hClose)
import System.Process (proc, readCreateProcessWithExitCode)
import PythonHS.CLI (runFile, replEvalLines)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

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

  it "uses vm engine for runFile when PYTHON_HS_RUNNER_ENGINE=vm" $
    withSystemTempFile "vm-engine.pyhs" $ \path h -> do
      hPutStr h "def add(a, b = 2):\n  return a + b\nprint add(1)\n"
      hClose h
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile path
        res `shouldSatisfy` \result ->
          case result of
            Left msg -> "VM compile error: unsupported statement" `isInfixOf` msg
            Right _ -> False

  it "falls back to ast engine when PYTHON_HS_RUNNER_ENGINE is unknown" $
    withSystemTempFile "ast-fallback.pyhs" $ \path h -> do
      hPutStr h "def add(a, b = 2):\n  return a + b\nprint add(1)\n"
      hClose h
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "unknown"
        res <- runFile path
        res `shouldBe` Right ["3"]

  it "reports unsupported mode for replEvalLines when PYTHON_HS_RUNNER_ENGINE=vm" $
    bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
      setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
      outs <- replEvalLines ["print 1", ""]
      outs `shouldBe` ["Error: VM engine is not supported in REPL yet"]

  it "returns an IO error for missing file" $ do
    res <- runFile "this-file-does-not-exist-xyz"
    case res of
      Left _ -> return ()
      Right _ -> error "expected Left for missing file"

  it "preserves environment across REPL inputs" $ do
    outs <- replEvalLines ["x = 1", "print x", ""]
    outs `shouldBe` ["1"]

  it "prints expression result for single-line REPL input" $ do
    outs <- replEvalLines ["1 + 2", ""]
    outs `shouldBe` ["3"]

  it "prints expression result using existing environment in REPL" $ do
    outs <- replEvalLines ["x = 10", "x + 5", ""]
    outs `shouldBe` ["15"]

  it "does not print expression result when it is None in REPL" $ do
    outs <- replEvalLines ["None", "1", ""]
    outs `shouldBe` ["1"]

  it "prints string expression result with quotes in REPL" $ do
    outs <- replEvalLines ["\"hello\"", ""]
    outs `shouldBe` ["'hello'"]

  it "prints list expression result using repr-style element formatting in REPL" $ do
    outs <- replEvalLines ["[\"a\", 1]", ""]
    outs `shouldBe` ["['a', 1]"]

  it "escapes tab and backslash in string expression result in REPL" $ do
    outs <- replEvalLines ["\"a\tb\"", "\"a\\b\"", ""]
    outs `shouldBe` ["'a\\tb'", "'a\\\\b'"]

  it "escapes single quote in string expression result in REPL" $ do
    outs <- replEvalLines ["\"a'b\"", ""]
    outs `shouldBe` ["'a\\'b'"]

  it "prints dictionary string keys with quotes in REPL" $ do
    outs <- replEvalLines ["{\"k\": 1, \"x\": 2}", ""]
    outs `shouldBe` ["{'k': 1, 'x': 2}"]

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

  it "ignores leading and interstitial blank lines in REPL" $ do
    outs <- replEvalLines ["", "", "x = 2", "", "print x", "", ""]
    outs `shouldBe` ["2"]

  it "submits a block on EOF even without trailing blank line (replEvalLines)" $ do
    outs <- replEvalLines ["def id(v):", "  return v", "print id(7)"]
    outs `shouldBe` ["7"]

  it "continues evaluating later lines after a runtime error in REPL" $ do
    outs <- replEvalLines ["print 1 / 0", "print 9", ""]
    outs `shouldBe` ["Error: Value error: division by zero at 1:9", "9"]

  it "stops REPL evaluation when exit() is entered" $ do
    outs <- replEvalLines ["print 1", "exit()", "print 2", ""]
    outs `shouldBe` ["1"]

  it "keeps environment after a failed block submission in REPL" $ do
    outs <- replEvalLines ["x = 10", "if x:", "", "print x", ""]
    outs `shouldSatisfy` \xs ->
      case xs of
        [err, out] -> "Error: " `isInfixOf` err && out == "10"
        _ -> False

  it "flushes pending block on EOF in interactive REPL executable" $ do
    (code, out, _err) <- readCreateProcessWithExitCode (proc "cabal" ["run", "-v0", "exe:python-hs"]) "def id(v):\n  return v\nprint id(3)\n"
    code `shouldBe` ExitSuccess
    ("3" `isInfixOf` out) `shouldBe` True

  it "shows initial prompt and re-shows prompt after an error in interactive REPL executable" $ do
    (code, out, _err) <- readCreateProcessWithExitCode (proc "cabal" ["run", "-v0", "exe:python-hs"]) "x @ 1\nprint 2\n"
    code `shouldBe` ExitSuccess
    (">>> " `isPrefixOf` out) `shouldBe` True
    ("Error: UnexpectedCharacter '@'\n>>> 2" `isInfixOf` out) `shouldBe` True

  it "exits interactive REPL when exit() is entered" $ do
    (code, out, _err) <- readCreateProcessWithExitCode (proc "cabal" ["run", "-v0", "exe:python-hs"]) "print 1\nexit()\nprint 2\n"
    code `shouldBe` ExitSuccess
    ("1" `isInfixOf` out) `shouldBe` True
    ("2" `isInfixOf` out) `shouldBe` False

  it "prints expression result in interactive REPL executable" $ do
    (code, out, _err) <- readCreateProcessWithExitCode (proc "cabal" ["run", "-v0", "exe:python-hs"]) "x = 10\nx + 5\nlen([1, 2]) + 3\n"
    code `shouldBe` ExitSuccess
    (">>> 15" `isInfixOf` out) `shouldBe` True
    (">>> 5" `isInfixOf` out) `shouldBe` True

  it "does not print None expression result in interactive REPL executable" $ do
    (code, out, _err) <- readCreateProcessWithExitCode (proc "cabal" ["run", "-v0", "exe:python-hs"]) "None\n1\n"
    code `shouldBe` ExitSuccess
    (">>> >>> 1" `isInfixOf` out) `shouldBe` True

  it "prints string expression result with quotes in interactive REPL executable" $ do
    (code, out, _err) <- readCreateProcessWithExitCode (proc "cabal" ["run", "-v0", "exe:python-hs"]) "\"hello\"\n"
    code `shouldBe` ExitSuccess
    (">>> 'hello'" `isInfixOf` out) `shouldBe` True

  it "escapes special characters in string expression result in interactive REPL executable" $ do
    (code, out, _err) <- readCreateProcessWithExitCode (proc "cabal" ["run", "-v0", "exe:python-hs"]) "\"a'b\"\n\"a\tb\"\n"
    code `shouldBe` ExitSuccess
    (">>> 'a\\'b'" `isInfixOf` out) `shouldBe` True
    (">>> 'a\\tb'" `isInfixOf` out) `shouldBe` True

  it "prints dictionary string keys with quotes in interactive REPL executable" $ do
    (code, out, _err) <- readCreateProcessWithExitCode (proc "cabal" ["run", "-v0", "exe:python-hs"]) "{\"k\": 1}\n"
    code `shouldBe` ExitSuccess
    (">>> {'k': 1}" `isInfixOf` out) `shouldBe` True
  where
    restoreRunnerEngine previous =
      case previous of
        Nothing -> unsetEnv "PYTHON_HS_RUNNER_ENGINE"
        Just value -> setEnv "PYTHON_HS_RUNNER_ENGINE" value