{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Text.IntellijImlSpec (spec) where

import           Test.Hspec

import           Text.RawString.QQ

import           Text.IntellijIml

spec :: Spec
spec = do
  describe "processing the xml file" $ do
    it "should keep the structure unchanged if no facet is present" $
      removeFacet "without_facet.iml" testIml `shouldBe` Right testIml
    it "should" $
      removeFacet "with_facet.iml" testImlWithFacet `shouldBe` Right testImlWithFacetRemoved


testIml :: String
testIml = [r|<?xml version='1.0' encoding='UTF-8' ?>
<module
  >
  <component name="FacetManager"
    >
    <facet type="Spring" name="Spring"
      >
      <configuration/>
    </facet>
  </component>
</module>|]

testImlWithFacet :: String
testImlWithFacet = [r|<?xml version='1.0' encoding='UTF-8'?>
<module>
  <component name="FacetManager">
    <facet type="Spring" name="Spring">
      <configuration />
    </facet>
    <facet type="gwt" name="GWT">
      <configuration>
        <setting name="gwtSdkUrl" value="file://path/to/sdk" />
        <setting name="gwtSdkType" value="gradle" />
      </configuration>
    </facet>
  </component>
</module>|]

testImlWithFacetRemoved :: String
testImlWithFacetRemoved = [r|<?xml version='1.0' encoding='UTF-8' ?>
<module
  >
  <component name="FacetManager"
    >
    <facet type="Spring" name="Spring"
      >
      <configuration/>
    </facet>
    
  </component>
</module>|]

