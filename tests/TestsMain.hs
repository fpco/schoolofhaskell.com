module TestsMain where

import           FoundationTest
import           GroupDescTest
import           Prelude
import           SecurityTokenTest
import qualified SerializationSpec
import           Test.Hspec (hspec)
import           TutorialTest

main :: IO ()
main = do
    -- (foundation, _) <- makeFoundation
    --     "github-client-id"
    --     "github-client-secret"
    --     (error "Sending email")
    hspec $ do
        foundationSpecs
        groupDescSpecs
        securityTokenSpecs
        SerializationSpec.spec
        tutorialSpecs
