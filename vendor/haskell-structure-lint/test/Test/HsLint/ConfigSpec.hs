module Test.HsLint.ConfigSpec (spec) where

import Data.ByteString.Lazy.Char8 qualified as BSL
import HsLint.Config (Config (..), decodeConfig, defaultConfig)
import HsLint.PositionalArgsConfig (positionalSkipFunctionPaths)
import HsLint.StructureConfig (structureMaxLines, structureRoots)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "decodeConfig" $ do
  it "returns the defaults for an empty object" $ do
    fmap configStructure (decodeConfig (BSL.pack "{}")) `shouldBe` Just (configStructure defaultConfig)
    fmap configPositionalArgs (decodeConfig (BSL.pack "{}")) `shouldBe` Just (configPositionalArgs defaultConfig)

  it "overrides structure fields" $ do
    let json = BSL.pack "{\"structure\":{\"roots\":[\"lib\"],\"maxLines\":120}}"
    fmap (structureRoots . configStructure) (decodeConfig json) `shouldBe` Just ["lib"]
    fmap (structureMaxLines . configStructure) (decodeConfig json) `shouldBe` Just 120

  it "keeps unspecified structure fields at their defaults" $ do
    let json = BSL.pack "{\"structure\":{\"maxLines\":120}}"
    fmap (structureRoots . configStructure) (decodeConfig json) `shouldBe` Just (structureRoots (configStructure defaultConfig))

  it "overrides positional argument fields" $ do
    let json = BSL.pack "{\"positionalArgs\":{\"skipFunctionPaths\":[\"/VM/\"]}}"
    fmap (positionalSkipFunctionPaths . configPositionalArgs) (decodeConfig json) `shouldBe` Just ["/VM/"]

  it "rejects invalid JSON" $ do
    decodeConfig (BSL.pack "not json") `shouldBe` Nothing
