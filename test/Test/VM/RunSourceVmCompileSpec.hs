module Test.VM.RunSourceVmCompileSpec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "compile builtin" $ do
  it "returns None for syntactically valid source" $ do
    runSourceVm "print(compile(\"x = 1\", \"<test>\", \"exec\"))\n"
      `shouldBe` Right ["None"]

  it "raises SyntaxError for invalid source" $ do
    runSourceVm
      ( unlines
          [ "try:",
            "    compile(\"def f(:\", \"<test>\", \"exec\")",
            "except SyntaxError:",
            "    print(\"caught\")"
          ]
      )
      `shouldBe` Right ["caught"]
