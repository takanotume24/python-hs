module Test.VM.RunSourceVmPart32Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  let counterClass =
        "class Counter:\n"
          ++ "    def __init__(self):\n"
          ++ "        self.i = 0\n"
          ++ "    def __iter__(self):\n"
          ++ "        return self\n"
          ++ "    def __next__(self):\n"
          ++ "        self.i += 1\n"
          ++ "        if self.i > 3:\n"
          ++ "            raise StopIteration\n"
          ++ "        return self.i\n"
   in describe "runSourceVm (vm mvp) - part 32" $ do
        describe "user-defined iteration protocol" $ do
          it "iterates with for through __iter__ and __next__" $ do
            runSourceVm (counterClass ++ "c = Counter()\nfor x in c:\n    print x\n")
              `shouldBe` Right ["1", "2", "3"]

          it "builds a list from a user iterable" $ do
            runSourceVm (counterClass ++ "c = Counter()\nprint list(c)\n")
              `shouldBe` Right ["[1, 2, 3]"]

          it "builds a tuple from a user iterable" $ do
            runSourceVm (counterClass ++ "c = Counter()\nprint tuple(c)\n")
              `shouldBe` Right ["(1, 2, 3)"]

          it "uses a user iterable in a comprehension" $ do
            runSourceVm (counterClass ++ "c = Counter()\nprint [x * 2 for x in c]\n")
              `shouldBe` Right ["[2, 4, 6]"]

          it "iterates a separate iterator object" $ do
            let source =
                  "class Items:\n"
                    ++ "    def __init__(self):\n"
                    ++ "        self.data = [10, 20]\n"
                    ++ "    def __iter__(self):\n"
                    ++ "        return Iterator(self.data)\n"
                    ++ "class Iterator:\n"
                    ++ "    def __init__(self, data):\n"
                    ++ "        self.data = data\n"
                    ++ "        self.index = 0\n"
                    ++ "    def __iter__(self):\n"
                    ++ "        return self\n"
                    ++ "    def __next__(self):\n"
                    ++ "        if self.index >= len(self.data):\n"
                    ++ "            raise StopIteration\n"
                    ++ "        value = self.data[self.index]\n"
                    ++ "        self.index += 1\n"
                    ++ "        return value\n"
            runSourceVm (source ++ "for x in Items():\n    print x\n")
              `shouldBe` Right ["10", "20"]

        describe "next builtin" $ do
          it "returns the next value and updates the iterator" $ do
            runSourceVm (counterClass ++ "c = Counter()\nprint next(c)\nprint next(c)\n")
              `shouldBe` Right ["1", "2"]

          it "returns the default when the iterator is exhausted" $ do
            runSourceVm
              ( counterClass
                  ++ "c = Counter()\nprint next(c)\nprint next(c)\nprint next(c)\nprint next(c, -1)\nprint next(c, -1)\n"
              )
              `shouldBe` Right ["1", "2", "3", "-1", "-1"]

          it "propagates StopIteration without a default" $ do
            runSourceVm
              ( counterClass
                  ++ "c = Counter()\nc.i = 3\ntry:\n    next(c)\nexcept StopIteration:\n    print 'stop'\n"
              )
              `shouldBe` Right ["stop"]

        describe "type errors" $ do
          it "rejects a class without __iter__ in for" $ do
            runSourceVm "class Plain:\n    pass\nfor x in Plain():\n    print x\n"
              `shouldBe` Left "Type error: Plain is not iterable at 3:10"

          it "rejects a class without __iter__ in list" $ do
            runSourceVm "class Plain:\n    pass\nprint list(Plain())\n"
              `shouldBe` Left "Type error: Plain is not iterable at 3:7"

          it "rejects an iterator without __next__ in next" $ do
            runSourceVm "class Plain:\n    pass\nprint next(Plain())\n"
              `shouldBe` Left "Type error: Plain is not an iterator at 3:7"

          it "rejects next on a non-object" $ do
            runSourceVm "print next(5)\n"
              `shouldBe` Left "Type error: int is not an iterator at 1:7"
