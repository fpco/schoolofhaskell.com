module GroupDescTest (groupDescSpecs) where

import Import.GroupDesc
import Prelude
import Test.Hspec

groupDescSpecs :: Spec
groupDescSpecs = describe "GroupDesc" $ do
    it "blank string external" $ externalDesc "" `shouldBe` ""
    it "blank string internal" $ internalDesc "" `shouldBe` ""
    it "plain string external" $ externalDesc "foo" `shouldBe` "foo"
    it "plain string internal" $ internalDesc "foo" `shouldBe` "foo"
    it "split string external" $ externalDesc "foo\n---\nbar" `shouldBe` "foo"
    it "split string internal" $ internalDesc "foo\n---\nbar" `shouldBe` "bar"
