module Test.VM.RunSourceVmPart37Spec (spec) where

import PythonHS.RunSourceVmWithSearchPaths (runSourceVmWithSearchPaths)
import PythonHS.RunSourceVmWithSearchPathsConfig (RunSourceVmWithSearchPathsConfig (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = describe "runSourceVmWithSearchPaths (cpython shims)" $ do
  describe "gc shim" $ do
    it "collects and reports tracked objects" $ do
      runVm
        ( unlines
            [ "import gc",
              "print gc.collect()",
              "print gc.is_tracked(1)",
              "print gc.garbage"
            ]
        )
        `shouldReturn` Right ["0", "1", "[]"]

    it "exposes the threshold tuple through a from-import" $ do
      runVm
        ( unlines
            [ "from gc import get_threshold",
              "print get_threshold()"
            ]
        )
        `shouldReturn` Right ["(700, 10, 10)"]

  describe "array shim" $ do
    it "builds a list-backed array with length and indexing" $ do
      runVm
        ( unlines
            [ "from array import array",
              "a = array('B', [1, 2, 3])",
              "print len(a)",
              "print a[1]",
              "print array('i', [4, 5])"
            ]
        )
        `shouldReturn` Right ["3", "2", "[4, 5]"]

  describe "itertools shim" $ do
    it "offers chain, product and islice" $ do
      runVm
        ( unlines
            [ "import itertools",
              "print itertools.chain([1, 2], [3])",
              "print itertools.product([1, 2], [3, 4])",
              "print itertools.product([1, 2], repeat=2)",
              "print itertools.islice([0, 1, 2, 3, 4], 1, 4, 2)"
            ]
        )
        `shouldReturn` Right
          [ "[1, 2, 3]",
            "[(1, 3), (1, 4), (2, 3), (2, 4)]",
            "[(1, 1), (1, 2), (2, 1), (2, 2)]",
            "[1, 3]"
          ]

    it "offers combinations, zip_longest and repeat" $ do
      runVm
        ( unlines
            [ "from itertools import combinations, zip_longest, repeat",
              "print combinations([1, 2, 3], 2)",
              "print zip_longest([1, 2], [3], [0])",
              "print repeat(7, 3)"
            ]
        )
        `shouldReturn` Right
          [ "[(1, 2), (1, 3), (2, 3)]",
            "[(1, 3, 0), (2, None, None)]",
            "[7, 7, 7]"
          ]

  describe "builtins shim" $ do
    it "exposes the module dictionary and marker name" $ do
      runVm
        ( unlines
            [ "import builtins",
              "print builtins.__dict__",
              "print builtins.here"
            ]
        )
        `shouldReturn` Right ["{}", "None"]

  describe "test.support shim" $ do
    it "exposes cpython_only and ALWAYS_EQ" $ do
      runVm
        ( unlines
            [ "from test.support import cpython_only, ALWAYS_EQ",
              "print cpython_only(7)",
              "print ALWAYS_EQ is ALWAYS_EQ"
            ]
        )
        `shouldReturn` Right ["7", "1"]

  describe "shim resolution" $ do
    it "prefers the shim path over later search paths" $ withSystemTempDirectory "python-hs-shim" $ \dir -> do
      writeFile (dir </> "keyword.py") "def iskeyword(word):\n    return False\n"
      runVmWithPaths
        ["cpython-shim", dir]
        ( unlines
            [ "import keyword",
              "print keyword.iskeyword('if')"
            ]
        )
        `shouldReturn` Right ["1"]

    it "uses a later path when the shim path has no module" $ withSystemTempDirectory "python-hs-shim" $ \dir -> do
      writeFile (dir </> "keyword.py") "def iskeyword(word):\n    return False\n"
      runVmWithPaths
        [dir]
        ( unlines
            [ "import keyword",
              "print keyword.iskeyword('if')"
            ]
        )
        `shouldReturn` Right ["0"]

    it "reports a member missing from a shim module" $ do
      runVm "from gc import missing\n"
        `shouldReturn` Left "Import error: name not found missing in gc"
  where
    runVm source =
      runVmWithPaths ["cpython-shim"] source

    runVmWithPaths paths source =
      runSourceVmWithSearchPaths
        RunSourceVmWithSearchPathsConfig
          { runSourceVmWithSearchPathsSearchPaths = paths,
            runSourceVmWithSearchPathsSource = source
          }
