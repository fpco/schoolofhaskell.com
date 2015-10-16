module FoundationTest
    ( foundationSpecs
    ) where

import Data.Map (empty, singleton, fromList)
import Foundation (updateSessionMap)
import Prelude
import Test.Hspec

foundationSpecs :: Spec
foundationSpecs = describe "Foundation" $ do
    describe "updateSessionMap" $ do
        let test old new curr final = it (show (old, new, curr)) $
                fmap ($ curr) (updateSessionMap old new) `shouldBe` final
        test
            (singleton "foo" "bar")
            (singleton "foo" "bar")
            (singleton "baz" "bin")
            Nothing
        test
            empty
            (singleton "foo" "bar")
            (singleton "baz" "bin")
            (Just $ fromList [("foo", "bar"), ("baz", "bin")])
        test
            (singleton "foo" "bar")
            empty
            (singleton "foo" "bin")
            (Just $ fromList [])
