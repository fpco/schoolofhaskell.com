module TutorialTest
    ( tutorialSpecs
    ) where

import Import.Tutorial
import Prelude
import Test.Hspec

tutorialSpecs :: Spec
tutorialSpecs = do
    describe "Tutorial" $ do
        it "nests" $ do
            let nodes =
                    [ TocNode "foo" "" 1
                    , TocNode "bar" "" 2
                    , TocNode "baz" "" 3
                    , TocNode "bin" "" 2
                    , TocNode "bak" "" 1
                    ]
                tree =
                    [ TocTree "foo" ""
                        [ TocTree "bar" ""
                            [ TocTree "baz" "" []
                            ]
                        , TocTree "bin" "" []
                        ]
                    , TocTree "bak" "" []
                    ]
            nestTocNodes nodes `shouldBe` tree
