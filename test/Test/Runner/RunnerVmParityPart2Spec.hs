{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Runner.RunnerVmParityPart2Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import PythonHS.Runner (runSource)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSource and runSourceVm parity (mvp subset) - edges" $ do
  it "matches for break and continue behavior" $ do
    shouldMatchVm "x = 0\nwhile x < 5:\n  x = x + 1\n  if x == 2:\n    continue\n  if x == 4:\n    break\n  print x\n"

  it "matches for global update and compound assignments" $ do
    shouldMatchVm "x = 10\ndef setGlobal():\n  global x\n  x = 99\n  return x\nprint setGlobal()\nprint x\ny = 1\ny += 2\ny *= 3\nprint y\n"

  it "matches for float literals and multiplicative operators" $ do
    shouldMatchVm "print 1.\nprint .5\nprint 1e3\nprint 1.2e-3\nprint 7 / 2\nprint 7 // 2\nprint 7 % 4\n"

  it "matches for nested suites with pass and later statements" $ do
    shouldMatchVm "x = 0\nif x:\n  pass\n  print 1\nelif 1:\n  pass\n  print 2\nelse:\n  pass\n  print 3\nprint 9\n"

  it "matches for truthiness of string list dict and None" $ do
    shouldMatchVm "if \"\":\n  print 1\nelse:\n  print 2\nif []:\n  print 3\nelse:\n  print 4\nif {}:\n  print 5\nelse:\n  print 6\nprint not \"\"\nprint not [1]\nprint not None\n"

  it "matches for global and pass statements" $ do
    shouldMatchVm "global x\nx = 1\npass\nif 1:\n  pass\nprint x\n"

  it "matches for indented while and for suites" $ do
    shouldMatchVm "x = 0\nwhile x < 3:\n  pass\n  x = x + 1\nprint x\nsum = 0\nfor i in [1, 2]:\n  pass\n  sum += i\nprint sum\n"

  it "matches for list builtins" $ do
    shouldMatchVm "print append([1, 2], 3)\nprint sort([3, 1.5, 2])\nprint reverse([1, 2, 3])\nprint remove([1, 2, 1], 1)\nprint insert([1, 3], 1, 2)\nprint pop([1, 2, 3])\n"

  it "matches for method-call style builtins" $ do
    shouldMatchVm "x = [1, 2]\nprint x.append(3)\nprint x.sort()\nd = {1: 2}\nprint d.setdefault(3, 4)\n"

  it "matches for dict builtins" $ do
    shouldMatchVm "print clear({1: 10})\nprint keys({2: 20, 1: 10})\nprint get({1: 10}, 1)\nprint update({1: 10}, 2, 20)\nprint setdefault({1: 10}, 2)\nprint values({2: 20, 1: 10})\nprint items({2: 20, 1: 10})\n"

  it "matches for dictionary insertion order builtins" $ do
    shouldMatchVm "print keys({3: 30, 1: 10})\nprint values({3: 30, 1: 10})\nprint items({3: 30, 1: 10})\n"

  it "matches for method-call variants of dict and list builtins" $ do
    shouldMatchVm "d = {1: 2, 3: 4}\nprint d.values()\nprint d.items()\nprint d.get(9, 99)\nprint d.pop(9, 77)\nprint d.update({3: 9, 5: 6})\nprint d.clear()\nx = [1, 2, 3]\nprint x.pop()\nprint x.clear()\n"

  it "matches for range start stop step forms" $ do
    shouldMatchVm "print range(2, 5)\nprint range(5, 0, -2)\n"

  it "matches for representative runtime errors" $ do
    shouldMatchVm "print len(1)\n"
    shouldMatchVm "print range(1, 2, 0)\n"
    shouldMatchVm "print 8 / 0\n"
    shouldMatchVm "for i in \"abc\":\n  print i\n"
    shouldMatchVm "print remove([1], 9)\n"
    shouldMatchVm "break\n"
    shouldMatchVm "continue\n"

  it "matches for representative lexer and parse errors" $ do
    shouldMatchVm "x @ 1\n"
    shouldMatchVm "if 1:\n  print 1\n print 2\n"
    shouldMatchVm "\tprint 1\n"
    shouldMatchVm "x\t=\t1\nprint x\n"
    shouldMatchVm "x = 2\nprint\tx\n"
    shouldMatchVm "if 1\n"
  where
    shouldMatchVm source = runSourceVm source `shouldBe` runSource source
