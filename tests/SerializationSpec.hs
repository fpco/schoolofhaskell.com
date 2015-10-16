{-# OPTIONS_GHC -fno-warn-orphans #-}

module SerializationSpec (spec) where

import qualified Data.ByteString as BS
import           Data.DeriveTH (derives, makeArbitrary)
import qualified Data.Text as T
import           Database.Persist.Sql
import           Model.Types
import           Prelude
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

instance Arbitrary T.Text where
    arbitrary = fmap T.pack arbitrary

instance Arbitrary BS.ByteString where
    arbitrary = fmap BS.pack arbitrary

instance Arbitrary BlobSHA where
    arbitrary = fmap BlobSHA arbitrarySHA

instance Arbitrary CommitSHA where
    arbitrary = fmap CommitSHA arbitrarySHA

arbitrarySHA :: Gen T.Text
arbitrarySHA = fmap T.pack $ vectorOf 40 $ elements (['0'..'9'] ++ ['a'..'f'])

-- FUTURE: Wouldn't it be cool if this boilerplate could be generated
-- from the models file?  In other words, all types used in the schema
-- would be tested.

$(derives [makeArbitrary]
  [ ''SHAPair
  , ''SSHKeyPair
  , ''SkillLevel
  ])

spec :: Spec
spec = describe "Model serialization roundtrips" $ do
    prop "SHAPair" $ roundtrips (a :: SHAPair)
    prop "SSHKeyPair" $ roundtrips (a :: SSHKeyPair)
    prop "SkillLevel" $ roundtrips (a :: SkillLevel)

a :: a
a = error "This shouldn't be evaluated"

-- Note: input just used to specify the type.
roundtrips :: forall a. (PersistField a, Arbitrary a, Eq a, Show a)
           => a -> Property
roundtrips _ =
    forAll (arbitrary :: Gen a) $ \x ->
        fromPersistValue (toPersistValue x) == Right x
