module Test.VM.RunSourceVmPart25Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 25" $ do
  describe "string method syntax" $ do
    it "splits with the receiver as the first argument" $ do
      runSourceVm "print 'a,b'.split(',')\n" `shouldBe` Right ["[a, b]"]

    it "joins with the receiver as the separator" $ do
      runSourceVm "print '-'.join(['a', 'b'])\n" `shouldBe` Right ["a-b"]

    it "calls case and strip methods" $ do
      runSourceVm "print 'AbC'.lower()\nprint 'AbC'.upper()\nprint '  x  '.strip()\n"
        `shouldBe` Right ["abc", "ABC", "x"]

    it "calls replace, find, count and startswith methods" $ do
      runSourceVm "print 'abc'.replace('a', 'z')\nprint 'abc'.find('b')\nprint 'abc'.count('b')\nprint 'abc'.startswith('a')\n"
        `shouldBe` Right ["zbc", "1", "1", "1"]

    it "matches the function call form" $ do
      runSourceVm "print 'a,b'.split(',') == split('a,b', ',')\nprint '-'.join(['a', 'b']) == join('-', ['a', 'b'])\n"
        `shouldBe` Right ["1", "1"]

  describe "list method syntax" $ do
    it "calls sort and reverse methods" $ do
      runSourceVm "print [3, 1, 2].sort()\nprint [1, 2, 3].reverse()\n"
        `shouldBe` Right ["[1, 2, 3]", "[3, 2, 1]"]

    it "calls append, extend and insert methods" $ do
      runSourceVm "print [1, 2].append(3)\nprint [1, 2].extend([3, 4])\nprint [1, 3].insert(1, 2)\n"
        `shouldBe` Right ["[1, 2, 3]", "[1, 2, 3, 4]", "[1, 2, 3]"]

    it "calls remove, pop and clear methods" $ do
      runSourceVm "print [1, 2, 3].remove(2)\nprint [1, 2, 3].pop()\nprint [1, 2].clear()\n"
        `shouldBe` Right ["[1, 3]", "3", "[]"]

    it "calls index, count and copy methods" $ do
      runSourceVm "print [10, 20].index(20)\nprint [1, 2, 2].count(2)\nprint [1, 2].copy()\n"
        `shouldBe` Right ["1", "2", "[1, 2]"]

    it "matches the function call form" $ do
      runSourceVm "print [3, 1, 2].sort() == sort([3, 1, 2])\nprint [1, 2].append(3) == append([1, 2], 3)\n"
        `shouldBe` Right ["1", "1"]

  describe "dict method syntax" $ do
    it "calls keys, values and items methods" $ do
      runSourceVm "d = {'a': 1, 'b': 2}\nprint d.keys()\nprint d.values()\nprint d.items()\n"
        `shouldBe` Right ["[a, b]", "[1, 2]", "[[a, 1], [b, 2]]"]

    it "calls get with and without a default" $ do
      runSourceVm "d = {'a': 1}\nprint d.get('a')\nprint d.get('z', 0)\n"
        `shouldBe` Right ["1", "0"]

    it "calls update, setdefault and pop methods" $ do
      runSourceVm "d = {'a': 1}\nprint d.update('b', 2)\nprint d.setdefault('c', 3)\nprint d.pop('a')\n"
        `shouldBe` Right ["{a: 1, b: 2}", "{a: 1, c: 3}", "1"]

    it "calls clear, copy and popitem methods" $ do
      runSourceVm "d = {'a': 1, 'b': 2}\nprint d.clear()\ne = {'a': 1}\nprint e.copy()\nprint e.popitem()\n"
        `shouldBe` Right ["{}", "{a: 1}", "(a, 1)"]

    it "matches the function call form" $ do
      runSourceVm "print {'a': 1}.keys() == keys({'a': 1})\nprint {'a': 1}.values() == values({'a': 1})\n"
        `shouldBe` Right ["1", "1"]
