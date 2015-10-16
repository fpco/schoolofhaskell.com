module SecurityTokenTest
    ( securityTokenSpecs
    ) where

import Data.Text (pack)
import Model
import Prelude
import Test.Hspec
import Test.Hspec.QuickCheck

securityTokenSpecs :: Spec
securityTokenSpecs = do
    describe "SecurityToken" $ do
        prop "equals itself" $ \str -> SecurityToken (pack str) == SecurityToken (pack str)
        prop "correct equality" $ \str1 str2 ->
            (str1 == str2) ==
            (SecurityToken (pack str1) == SecurityToken (pack str2))
