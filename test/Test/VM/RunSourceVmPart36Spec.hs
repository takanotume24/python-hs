module Test.VM.RunSourceVmPart36Spec (spec) where

import PythonHS.RunSourceVmWithSearchPaths (runSourceVmWithSearchPaths)
import PythonHS.RunSourceVmWithSearchPathsConfig (RunSourceVmWithSearchPathsConfig (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = describe "runSourceVmWithSearchPaths (stdlib imports)" $ do
  describe "builtin stdlib modules" $ do
    it "resolves import sys through the search-path pipeline" $ do
      runVm
        ( unlines
            [ "import sys",
              "print sys.platform",
              "print sys.maxsize"
            ]
        )
        `shouldReturn` Right ["linux", "9223372036854775807"]

    it "resolves from sys import members" $ do
      runVm
        ( unlines
            [ "from sys import maxsize, byteorder",
              "print maxsize",
              "print byteorder"
            ]
        )
        `shouldReturn` Right ["9223372036854775807", "little"]

    it "resolves import time and time.time()" $ do
      runVm
        ( unlines
            [ "import time",
              "print time.time() > 0"
            ]
        )
        `shouldReturn` Right ["1"]

    it "resolves from time import sleep" $ do
      runVm
        ( unlines
            [ "from time import sleep",
              "print sleep(0)"
            ]
        )
        `shouldReturn` Right ["None"]

    it "resolves import random and randint" $ do
      runVm
        ( unlines
            [ "import random",
              "print random.randint(3, 3)"
            ]
        )
        `shouldReturn` Right ["3"]

    it "resolves from random import choice" $ do
      runVm
        ( unlines
            [ "from random import choice",
              "print choice([9])"
            ]
        )
        `shouldReturn` Right ["9"]

    it "resolves aliased sys import" $ do
      runVm
        ( unlines
            [ "import sys as s",
              "print s.byteorder"
            ]
        )
        `shouldReturn` Right ["little"]

  describe "other stdlib from-imports" $ do
    it "resolves from os import getcwd" $ do
      runVm
        ( unlines
            [ "from os import getcwd",
              "print getcwd()"
            ]
        )
        `shouldReturn` Right ["."]

    it "resolves from json import loads" $ do
      runVm
        ( unlines
            [ "from json import loads",
              "print loads('42')"
            ]
        )
        `shouldReturn` Right ["42"]

    it "resolves from pathlib import Path" $ do
      runVm
        ( unlines
            [ "from pathlib import Path",
              "print Path('sample')"
            ]
        )
        `shouldReturn` Right ["sample"]

  describe "resolution errors" $ do
    it "still resolves local modules from the search path" $ withSystemTempDirectory "python-hs-import" $ \dir -> do
      writeFile (dir </> "localmod.py") "value = 7\n"
      runVmWithPaths
        [dir]
        ( unlines
            [ "import localmod",
              "print localmod.value"
            ]
        )
        `shouldReturn` Right ["7"]

    it "reports an unsupported stdlib member" $ do
      runVm "from sys import missing\n"
        `shouldReturn` Left "Import error: unsupported module member missing at 1:1"

    it "reports an unknown module" $ do
      runVm "import no_such_module\n"
        `shouldReturn` Left "Import error: module not found no_such_module"
  where
    runVm source =
      runVmWithPaths [] source

    runVmWithPaths paths source =
      runSourceVmWithSearchPaths
        RunSourceVmWithSearchPathsConfig
          { runSourceVmWithSearchPathsSearchPaths = paths,
            runSourceVmWithSearchPathsSource = source
          }
