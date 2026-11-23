module Test.VM.RunSourceVmPart42Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm except* support" $ do
  it "catches an exception with an except* clause" $ do
    runSourceVm "try:\n    raise Exception('x')\nexcept* BaseException:\n    print 'caught'\n"
      `shouldBe` Right ["caught"]

  it "runs the else clause when no exception is raised" $ do
    runSourceVm "try:\n    pass\nexcept* BaseException:\n    print 'caught'\nelse:\n    print 'else'\n"
      `shouldBe` Right ["else"]

  it "runs the finally clause after an except* clause" $ do
    runSourceVm "try:\n    raise Exception('x')\nexcept* BaseException:\n    print 'caught'\nfinally:\n    print 'finally'\n"
      `shouldBe` Right ["caught", "finally"]

  it "keeps nested except* clauses independent" $ do
    runSourceVm "try:\n    try:\n        raise Exception('inner')\n    except* BaseException:\n        print 'inner'\nfinally:\n    print 'outer'\n"
      `shouldBe` Right ["inner", "outer"]
