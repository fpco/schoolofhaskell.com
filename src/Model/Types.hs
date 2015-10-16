{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.Types where

import           ClassyPrelude.Yesod hiding (show, Read)
import           Data.Aeson (withObject)
import qualified Data.ByteString.Base64 as B64
import qualified Data.Serialize as Cereal
import qualified Data.Text
import           Database.Persist.Sql as DB
import           Network.Mail.Mime (randomString)
import           Prelude (Show (show), Read (readsPrec))
import           System.Random (Random, randomR, random)
import           Text.Blaze.Html (ToMarkup (..))
import           Data.Data

-- | A Git blob SHA in textual form.
newtype BlobSHA = BlobSHA { unBlobSHA :: Text }
    deriving (Show, Eq, Read, Ord, Generic, Data, Typeable,
              ToJSON, FromJSON,
              PersistField,
              PathPiece, ToMarkup, ToMessage)

instance PersistFieldSql BlobSHA where
    sqlType = sqlType . liftM unBlobSHA

-- | A Git commit SHA in textual form.
newtype CommitSHA = CommitSHA { unCommitSHA :: Text }
    deriving (Show, Eq, Read, Ord, Generic, Data, Typeable,
              ToJSON, FromJSON,
              PersistField,
              PathPiece, ToMarkup, ToMessage)

instance PersistFieldSql CommitSHA where
    sqlType = sqlType . liftM unCommitSHA

data SHAPair = SHAPair !CommitSHA !BlobSHA
    deriving (Show, Read, Eq)

instance PersistField SHAPair where
    toPersistValue (SHAPair x y) =
        toPersistValue $ unwords [toPathPiece x, unBlobSHA y]
    fromPersistValue v =
        case fromPersistValue v of
            Left e -> Left e
            Right t | [x, y] <-
                words t -> Right $ SHAPair (CommitSHA x) (BlobSHA y)
            Right _ -> Left "Invalid SHAPair"
instance PersistFieldSql SHAPair where
    sqlType _ = SqlString

newtype Title = Title { unTitle :: Text }
    deriving (Show, Eq, Read, Ord, Generic, Data, Typeable,
              ToJSON, FromJSON,
              PersistField,
              PathPiece, ToMarkup, ToMessage)
instance PersistFieldSql Title where
    sqlType = sqlType . liftM unTitle

newtype UserHandle = UserHandle { unUserHandle :: Text }
    deriving (Show, Eq, Read, Ord, Generic, Data, Typeable,
              ToJSON, FromJSON,
              PersistField,
              PathPiece, ToMarkup, ToMessage)
instance PersistFieldSql UserHandle where
    sqlType = sqlType . liftM unUserHandle

newtype NormalizedHandle = NormalizedHandle { unNormalizedHandle :: Text }
    deriving (Show, Eq, Read, Ord, Generic, Data, Typeable,
              ToJSON, FromJSON,
              PersistField,
              PathPiece, ToMarkup, ToMessage)
instance PersistFieldSql NormalizedHandle where
    sqlType = sqlType . liftM unNormalizedHandle

newtype TutorialName = TutorialName { unTutorialName :: Text }
    deriving (Show, Eq, Read, Ord, Generic, Data, Typeable,
              ToJSON, FromJSON,
              PersistField,
              PathPiece, ToMarkup, ToMessage)
instance PersistFieldSql TutorialName where
    sqlType = sqlType . liftM unTutorialName

newtype TutorialContent = TutorialContent' { unTutorialContent :: Text }
    deriving (Show, Eq, Read, Ord, Generic, Data, Typeable,
              ToJSON, FromJSON,
              PersistField,
              PathPiece, ToMarkup, ToMessage)
instance PersistFieldSql TutorialContent where
    sqlType = sqlType . liftM unTutorialContent

newtype SecurityToken = SecurityToken { unSecurityToken :: Text }
    deriving (Show, Read, Ord, Generic, Data, Typeable,
              ToJSON, FromJSON,
              PersistField,
              PathPiece, ToMarkup, ToMessage)
instance PersistFieldSql SecurityToken where
    sqlType = sqlType . liftM unSecurityToken
-- | Constant time equality to avoid timing attacks.
instance Eq SecurityToken where
    SecurityToken x == SecurityToken y =
        all (uncurry (==)) (Data.Text.zip x y) && length x == length y

newtype DeletionToken = DeletionToken { unDeletionToken :: Text }
    deriving (Eq, Show, Read, Ord, Generic, Data, Typeable,
              ToJSON, FromJSON,
              PersistField,
              PathPiece, ToMarkup, ToMessage)
instance PersistFieldSql DeletionToken where
    sqlType = sqlType . liftM unDeletionToken

type TutorialNames = [TutorialName]
instance PathMultiPiece TutorialNames where
    toPathMultiPiece = map toPathPiece
    fromPathMultiPiece = mapM fromPathPiece

slugify :: IsSlug a => Text -> Either Text a
slugify t =
    case normalizeHandle (UserHandle t) of
        Left e -> Left $ pack $ show e
        Right (NormalizedHandle h) -> Right $ toSlug h

class IsSlug a where toSlug :: Text -> a
instance IsSlug TutorialName where toSlug = TutorialName

data NormalizeFailure = NFTooShort | NFInvalidCharacters
instance Show NormalizeFailure where
    show NFTooShort = "Must be at least three characters"
    show NFInvalidCharacters =
        "Must be the letters a-z, digits, hyphens and underscores"

normalizeHandle :: UserHandle -> Either NormalizeFailure NormalizedHandle
normalizeHandle (UserHandle t')
    | length t' < 3 = Left NFTooShort
    | all isValid t = Right $ NormalizedHandle t
    | otherwise = Left NFInvalidCharacters
  where
    t = toLower t'
    isValid c
        | 'a' <= c && c <= 'z' = True
        | '0' <= c && c <= '9' = True
        | c == '-' || c == '_' || c == '.' = True
        | otherwise = False

titleToSlug :: Title -> Text
titleToSlug =
    minLength . intercalate "-" . words . omap valid . toLower . unTitle
  where
    minLength x = take (max 3 $ length x) $ x ++ "123"
    valid c
        | 'a' <= c && c <= 'z' = c
        | '0' <= c && c <= '9' = c
        | otherwise = ' '

instance Random SecurityToken where
    randomR = const random
    random = first (SecurityToken . pack) . randomString 15

newtype GithubAccessKey = GithubAccessKey ByteString
    deriving (Show, Eq, Read, Generic, PersistField)
instance PersistFieldSql GithubAccessKey where
    sqlType = sqlType . liftM (\(GithubAccessKey v) -> v)
instance ToJSON GithubAccessKey where
    toJSON (GithubAccessKey bs) = toJSON $ decodeUtf8 $ B64.encode bs
instance FromJSON GithubAccessKey where
    parseJSON = fmap (GithubAccessKey . B64.decodeLenient . encodeUtf8) . parseJSON

data Keymap = KeymapVim | KeymapEmacs
    deriving (Show, Eq, Read, Enum, Bounded, Generic)
instance ToJSON Keymap
instance FromJSON Keymap

derivePersistField "Keymap"

data SSHKeyPair = SSHKeyPair
    { publicKey :: !ByteString
    , privateKey :: !ByteString
    }
    deriving (Show, Eq, Ord, Generic)
instance Cereal.Serialize SSHKeyPair
instance ToJSON SSHKeyPair where
    toJSON (SSHKeyPair pub priv) = object
        [ "public" .= decodeUtf8 (B64.encode pub)
        , "private" .= decodeUtf8 (B64.encode priv)
        ]
instance FromJSON SSHKeyPair where
    parseJSON = withObject "SSHKeyPair" $ \o -> SSHKeyPair
        <$> (go <$> (o .: "public"))
        <*> (go <$> (o .: "private"))
      where
        go = B64.decodeLenient . encodeUtf8
instance PersistField SSHKeyPair where
    toPersistValue = toPersistValue . Cereal.encode
    fromPersistValue =
        fromPersistValue >=> either (Left . pack) Right . Cereal.decode
instance PersistFieldSql SSHKeyPair where
    sqlType _ = SqlBlob

data SkillLevel = SLBeginner | SLAdvanced
    deriving (Eq, Ord, Enum, Bounded, Generic, Data, Typeable)
instance Show SkillLevel where
    show SLBeginner = "Beginner"
    show SLAdvanced = "Advanced"
instance Read SkillLevel where
    readsPrec _ s =
        case lookup s m of
            Nothing -> []
            Just sl -> [(sl, "")]
      where
        m = map (show &&& id) [minBound..maxBound]
instance ToJSON SkillLevel
instance FromJSON SkillLevel
derivePersistField "SkillLevel"

-- | Access code used for viewing tutorial previews.
type PreviewCode = Text

newtype LowerCaseText = LowerCaseTextDoNotUse { unLowerCaseText :: Text }
    deriving (Show, Eq, Read, Ord, Generic, Data, Typeable,
              ToJSON, FromJSON,
              PersistField,
              PathPiece, ToMarkup, ToMessage)
instance PersistFieldSql LowerCaseText where
    sqlType = sqlType . liftM unLowerCaseText

mkLowerCaseText :: Text -> LowerCaseText
mkLowerCaseText = LowerCaseTextDoNotUse . toLower

newtype PkgSetId = PkgSetId { unPkgSetId :: Text }
    deriving (Eq, Read, Show, Data, Typeable, Ord, PathPiece,
              ToJSON, FromJSON, Generic, Hashable,
              PersistField)
instance PersistFieldSql PkgSetId where
    sqlType = sqlType . liftM unPkgSetId
-- | work around persistent's relative table rules
type PkgSetId' = PkgSetId

newtype GhcEnvId = GhcEnvId { unGhcEnvId :: Text }
    deriving (Eq, Read, Show, Data, Typeable, Ord, PathPiece,
              ToJSON, FromJSON, Generic, Hashable,
              PersistField)
instance PersistFieldSql GhcEnvId where
    sqlType = sqlType . liftM unGhcEnvId
type GhcEnvId' = GhcEnvId

-- | Controls how Disqus comments are displayed for an individual's content.
data Disqus
    = NoComments
    | FPAccount -- ^ Use FP Complete's account
    | UserAccount DisqusIdent
    deriving (Show, Generic, Eq)

instance ToJSON Disqus
instance FromJSON Disqus
instance PersistField Disqus where
    toPersistValue NoComments = PersistText "nocomments"
    toPersistValue FPAccount = PersistText "fpaccount"
    toPersistValue (UserAccount di) = PersistText $ "user-" ++ unDisqusIdent di

    fromPersistValue pv = do
        t <- fromPersistValue pv
        case t of
            "nocomments" -> return NoComments
            "fpaccount"  -> return FPAccount
            _ -> do
                di' <- maybe (Left "Invalid Disqus value") Right
                     $ stripPrefix "user-" t
                either (Left . tshow) (Right . UserAccount) (mkDisqusIdent di')

instance PersistFieldSql Disqus where
    sqlType _ = SqlString

newtype DisqusIdent = DisqusIdent Text
    deriving (Show, Generic, Eq)

unDisqusIdent :: DisqusIdent -> Text
unDisqusIdent (DisqusIdent x) = x

newtype DisqusException = DisqusException Text
    deriving (Typeable, Generic)
instance Show DisqusException where
    show (DisqusException t) = unpack t
instance Exception DisqusException
instance ToJSON DisqusIdent
instance FromJSON DisqusIdent where
    parseJSON v = do
        t <- parseJSON v
        case mkDisqusIdent t of
            Left e -> fail $ show e
            Right x -> return x

-- FIXME need something more definitive
mkDisqusIdent :: MonadThrow m => Text -> m DisqusIdent
mkDisqusIdent "" = throwM $ DisqusException "Shortname must not be blank"
mkDisqusIdent t
    | length t > 50 = throwM $ DisqusException "Shortname must be at most 50 characters"
    | length t < 3 = throwM $ DisqusException "Shortname must less at least 3 characters"
    | any isValid t = return $ DisqusIdent t
    | otherwise = throwM $ DisqusException "Your shortname must be letters, numbers, hyphens and underscores only"
  where
    isValid c =
        ('A' <= c && c <= 'Z') ||
        ('a' <= c && c <= 'z') ||
        ('0' <= c && c <= '9') ||
        c == '-' ||
        c == '_'

-- | Token for a tutorial.
newtype TutorialConcurrentToken = TutorialConcurrentToken'
    { unTutorialConcurrentToken :: Int32 }
    deriving (Eq, Show, Data, Read, Typeable, Num, Ord, Generic,
              ToJSON, FromJSON, Hashable,
              PersistField, Random)

instance Default TutorialConcurrentToken where
    def = TutorialConcurrentToken' 1

instance PersistFieldSql TutorialConcurrentToken where
    sqlType = sqlType . liftM unTutorialConcurrentToken

incrToken :: TutorialConcurrentToken -> TutorialConcurrentToken
incrToken (TutorialConcurrentToken' x) = TutorialConcurrentToken' (x + 1)
