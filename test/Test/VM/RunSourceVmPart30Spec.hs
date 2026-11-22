module Test.VM.RunSourceVmPart30Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import PythonHS.RunSourceVmWithSearchPaths (runSourceVmWithSearchPaths)
import PythonHS.RunSourceVmWithSearchPathsConfig (RunSourceVmWithSearchPathsConfig (..))
import System.Directory (createDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 30" $ do
  describe "local module resolution" $ do
    it "imports a sibling module" $
      withSystemTempDirectory "vm-local-module" $ \dir -> do
        writeFile (dir </> "helper.py") "def inc(x):\n  return x + 1\n"
        result <- runIn dir "import helper\nprint helper.inc(4)\n"
        result `shouldBe` Right ["5"]

    it "imports a sibling module with an alias" $
      withSystemTempDirectory "vm-local-module-alias" $ \dir -> do
        writeFile (dir </> "helper.py") "def inc(x):\n  return x + 1\n"
        result <- runIn dir "import helper as h\nprint h.inc(4)\n"
        result `shouldBe` Right ["5"]

    it "imports a name from a sibling module" $
      withSystemTempDirectory "vm-local-from-module" $ \dir -> do
        writeFile (dir </> "helper.py") "def inc(x):\n  return x + 1\nbase = 10\n"
        result <- runIn dir "from helper import inc as f, base\nprint f(4)\nprint base\n"
        result `shouldBe` Right ["5", "10"]

    it "imports a package submodule by dotted path" $
      withSystemTempDirectory "vm-local-package-submodule" $ \dir -> do
        createDirectory (dir </> "pkg")
        writeFile (dir </> "pkg" </> "__init__.py") "pass\n"
        writeFile (dir </> "pkg" </> "sub.py") "def inc(x):\n  return x + 1\n"
        result <- runIn dir "import pkg.sub\nprint pkg.sub.inc(4)\n"
        result `shouldBe` Right ["5"]

    it "imports a submodule from a package" $
      withSystemTempDirectory "vm-local-from-package" $ \dir -> do
        createDirectory (dir </> "pkg")
        writeFile (dir </> "pkg" </> "__init__.py") "pass\n"
        writeFile (dir </> "pkg" </> "sub.py") "def inc(x):\n  return x + 1\n"
        result <- runIn dir "from pkg import sub\nprint sub.inc(4)\n"
        result `shouldBe` Right ["5"]

    it "imports a member from a package submodule" $
      withSystemTempDirectory "vm-local-from-submodule-member" $ \dir -> do
        createDirectory (dir </> "pkg")
        writeFile (dir </> "pkg" </> "__init__.py") "pass\n"
        writeFile (dir </> "pkg" </> "sub.py") "def inc(x):\n  return x + 1\n"
        result <- runIn dir "from pkg.sub import inc as i\nprint i(4)\n"
        result `shouldBe` Right ["5"]

    it "prefers a package attribute over a submodule" $
      withSystemTempDirectory "vm-local-attribute-priority" $ \dir -> do
        createDirectory (dir </> "pkg")
        writeFile (dir </> "pkg" </> "__init__.py") "name = 41\n"
        writeFile (dir </> "pkg" </> "name.py") "def value():\n  return 99\n"
        result <- runIn dir "from pkg import name\nprint name\n"
        result `shouldBe` Right ["41"]

    it "runs a module body only once across repeated imports" $
      withSystemTempDirectory "vm-local-import-once" $ \dir -> do
        writeFile (dir </> "helper.py") "print 'loaded'\nx = 1\n"
        result <- runIn dir "import helper\nimport helper\nprint helper.x\n"
        result `shouldBe` Right ["loaded", "1"]

    it "prefers a package over a same-named module file" $
      withSystemTempDirectory "vm-local-package-priority" $ \dir -> do
        writeFile (dir </> "choice.py") "value = 1\n"
        createDirectory (dir </> "choice")
        writeFile (dir </> "choice" </> "__init__.py") "value = 2\n"
        result <- runIn dir "import choice\nprint choice.value\n"
        result `shouldBe` Right ["2"]

    it "supports a module importing another module" $
      withSystemTempDirectory "vm-local-module-chain" $ \dir -> do
        writeFile (dir </> "base.py") "def one():\n  return 1\n"
        writeFile (dir </> "middle.py") "from base import one\ndef two():\n  return one() + 1\n"
        result <- runIn dir "import middle\nprint middle.two()\n"
        result `shouldBe` Right ["2"]

  describe "standard module from-import" $ do
    it "imports os.getcwd" $ do
      runSourceVm "from os import getcwd\nprint getcwd()\n" `shouldBe` Right ["."]

    it "imports os.path as a module value" $ do
      runSourceVm "from os import path\nprint path\n" `shouldBe` Right ["<module:os.path>"]

    it "imports json functions" $ do
      runSourceVm "from json import dumps, loads\nprint dumps(1)\nprint loads(\"1\")\n" `shouldBe` Right ["1", "1"]

    it "imports pathlib.Path" $ do
      runSourceVm "from pathlib import Path\nprint Path(\"a/b\")\n" `shouldBe` Right ["a/b"]

    it "supports aliases for standard module members" $ do
      runSourceVm "from json import dumps as d\nprint d(2)\n" `shouldBe` Right ["2"]

    it "reports an unsupported standard module member" $ do
      runSourceVm "from os import missing\n" `shouldBe` Left "Import error: unsupported module member missing at 1:1"

  describe "import errors" $ do
    it "reports a missing top-level module" $
      withSystemTempDirectory "vm-local-missing-module" $ \dir -> do
        result <- runIn dir "import missing\n"
        result `shouldBe` Left "Import error: module not found missing"

    it "reports a missing member" $
      withSystemTempDirectory "vm-local-missing-member" $ \dir -> do
        writeFile (dir </> "helper.py") "x = 1\n"
        result <- runIn dir "from helper import missing\n"
        result `shouldBe` Left "Import error: name not found missing in helper"

    it "reports a missing imported name through a package" $
      withSystemTempDirectory "vm-local-missing-package-member" $ \dir -> do
        createDirectory (dir </> "pkg")
        writeFile (dir </> "pkg" </> "__init__.py") "pass\n"
        result <- runIn dir "from pkg import missing\n"
        result `shouldBe` Left "Import error: name not found missing in pkg"

    it "supports a circular import with partially initialized modules" $
      withSystemTempDirectory "vm-local-circular-import" $ \dir -> do
        createDirectory (dir </> "pkg")
        writeFile (dir </> "pkg" </> "__init__.py") "pass\n"
        writeFile (dir </> "pkg" </> "a.py") "import pkg.b\ndef a():\n  return 1\n"
        writeFile (dir </> "pkg" </> "b.py") "import pkg.a\ndef b():\n  return 2\n"
        result <- runIn dir "from pkg.a import a\nprint a()\n"
        result `shouldBe` Right ["1"]

    it "reports a name that is not yet defined in a circular import" $
      withSystemTempDirectory "vm-local-circular-import-error" $ \dir -> do
        createDirectory (dir </> "pkg")
        writeFile (dir </> "pkg" </> "__init__.py") "pass\n"
        writeFile (dir </> "pkg" </> "a.py") "from pkg.b import b_value\na_value = 1\n"
        writeFile (dir </> "pkg" </> "b.py") "from pkg.a import a_value\nb_value = 2\n"
        result <- runIn dir "import pkg.a\n"
        result `shouldBe` Left "Import error: name not found a_value in pkg.a"
  where
    writeFile path contents = Prelude.writeFile path contents

    runIn dir src = do
      runSourceVmWithSearchPaths (RunSourceVmWithSearchPathsConfig {runSourceVmWithSearchPathsSearchPaths = [dir], runSourceVmWithSearchPathsSource = src})
