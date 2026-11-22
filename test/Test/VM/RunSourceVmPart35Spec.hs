module Test.VM.RunSourceVmPart35Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 35" $ do
  describe "sys module" $ do
    it "exposes argv and platform markers" $ do
      runSourceVm
        ( unlines
            [ "import sys",
              "print sys.argv",
              "print sys.platform"
            ]
        )
        `shouldBe` Right ["[]", "python-hs"]

    it "exposes path and version markers" $ do
      runSourceVm
        ( unlines
            [ "import sys",
              "print sys.path",
              "print sys.version"
            ]
        )
        `shouldBe` Right ["[.]", "python-hs 0.1.0"]

    it "supports from-import of sys attributes" $ do
      runSourceVm
        ( unlines
            [ "from sys import argv, platform",
              "print argv",
              "print platform"
            ]
        )
        `shouldBe` Right ["[]", "python-hs"]

    it "supports aliased import" $ do
      runSourceVm
        ( unlines
            [ "import sys as s",
              "print s.platform"
            ]
        )
        `shouldBe` Right ["python-hs"]

    it "rejects an unsupported sys member" $ do
      runSourceVm "from sys import missing\n"
        `shouldBe` Left "Import error: unsupported module member missing at 1:1"

  describe "time module" $ do
    it "returns a positive timestamp" $ do
      runSourceVm
        ( unlines
            [ "import time",
              "t = time.time()",
              "print t > 0"
            ]
        )
        `shouldBe` Right ["1"]

    it "supports from-import of time" $ do
      runSourceVm
        ( unlines
            [ "from time import time",
              "t = time()",
              "print t > 0"
            ]
        )
        `shouldBe` Right ["1"]

    it "sleeps and returns None" $ do
      runSourceVm
        ( unlines
            [ "import time",
              "print time.sleep(0)"
            ]
        )
        `shouldBe` Right ["None"]

    it "rejects a non-numeric sleep argument" $ do
      runSourceVm
        ( unlines
            [ "import time",
              "time.sleep('x')"
            ]
        )
        `shouldBe` Left "Type error: sleep expects number at 2:6"

    it "rejects a negative sleep duration" $ do
      runSourceVm
        ( unlines
            [ "import time",
              "time.sleep(-1)"
            ]
        )
        `shouldBe` Left "Value error: sleep length must be non-negative at 2:6"

  describe "random module" $ do
    it "returns the only value of a single-point range" $ do
      runSourceVm
        ( unlines
            [ "import random",
              "print random.randint(1, 1)"
            ]
        )
        `shouldBe` Right ["1"]

    it "keeps randint within its inclusive bounds" $ do
      runSourceVm
        ( unlines
            [ "import random",
              "x = random.randint(1, 3)",
              "print x >= 1 and x <= 3"
            ]
        )
        `shouldBe` Right ["1"]

    it "chooses from a single-element list" $ do
      runSourceVm
        ( unlines
            [ "import random",
              "print random.choice([7])"
            ]
        )
        `shouldBe` Right ["7"]

    it "chooses a character from a single-character string" $ do
      runSourceVm
        ( unlines
            [ "import random",
              "print random.choice('a')"
            ]
        )
        `shouldBe` Right ["a"]

    it "supports from-import of randint and choice" $ do
      runSourceVm
        ( unlines
            [ "from random import randint, choice",
              "print randint(2, 2)",
              "print choice([5])"
            ]
        )
        `shouldBe` Right ["2", "5"]

    it "rejects an empty randint range" $ do
      runSourceVm
        ( unlines
            [ "import random",
              "random.randint(1, 0)"
            ]
        )
        `shouldBe` Left "Value error: empty range for randint at 2:8"

    it "rejects non-int randint arguments" $ do
      runSourceVm
        ( unlines
            [ "import random",
              "random.randint(1, 'a')"
            ]
        )
        `shouldBe` Left "Type error: randint expects int arguments at 2:8"

    it "rejects choosing from an empty sequence" $ do
      runSourceVm
        ( unlines
            [ "import random",
              "random.choice([])"
            ]
        )
        `shouldBe` Left "Index error: cannot choose from an empty sequence at 2:8"

    it "rejects choosing from a non-sequence" $ do
      runSourceVm
        ( unlines
            [ "import random",
              "random.choice(1)"
            ]
        )
        `shouldBe` Left "Type error: choice expects a non-empty sequence at 2:8"
