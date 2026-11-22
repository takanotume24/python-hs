module Test.VM.RunSourceVmPart33Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 33" $ do
  describe "nonlocal rebinding" $ do
    it "rebinds an enclosing local from a nested function" $ do
      runSourceVm
        ( unlines
            [ "def outer():",
              "    x = 1",
              "    def inner():",
              "        nonlocal x",
              "        x = 2",
              "    inner()",
              "    print x",
              "outer()"
            ]
        )
        `shouldBe` Right ["2"]

    it "reads and updates an enclosing local across calls" $ do
      runSourceVm
        ( unlines
            [ "def outer():",
              "    x = 1",
              "    def inner():",
              "        nonlocal x",
              "        x = x + 1",
              "    inner()",
              "    inner()",
              "    print x",
              "outer()"
            ]
        )
        `shouldBe` Right ["3"]

    it "reads an enclosing local without rebinding it" $ do
      runSourceVm
        ( unlines
            [ "def outer():",
              "    x = 7",
              "    def inner():",
              "        nonlocal x",
              "        print x",
              "    inner()",
              "outer()"
            ]
        )
        `shouldBe` Right ["7"]

  describe "nonlocal errors" $ do
    it "rejects nonlocal at module level" $ do
      runSourceVm "nonlocal x\n"
        `shouldBe` Left "Syntax error: nonlocal declaration not allowed at module level at 1:1"

    it "rejects a nonlocal that is also a parameter" $ do
      runSourceVm
        ( unlines
            [ "def outer():",
              "    x = 1",
              "    def inner(x):",
              "        nonlocal x",
              "    inner(2)",
              "outer()"
            ]
        )
        `shouldBe` Left "Syntax error: name 'x' is parameter and nonlocal at 4:9"

    it "rejects a nonlocal without an enclosing binding" $ do
      runSourceVm
        ( unlines
            [ "def outer():",
              "    def inner():",
              "        nonlocal y",
              "        y = 1",
              "    inner()",
              "outer()"
            ]
        )
        `shouldBe` Left "Syntax error: no binding for nonlocal 'y' found at 3:9"

    it "rejects a name declared both global and nonlocal" $ do
      runSourceVm
        ( unlines
            [ "x = 0",
              "def outer():",
              "    x = 1",
              "    def inner():",
              "        global x",
              "        nonlocal x",
              "    inner()",
              "outer()"
            ]
        )
        `shouldBe` Left "Syntax error: name 'x' is nonlocal and global at 6:9"
