{-# LANGUAGE ConstraintKinds, DeriveDataTypeable, DeriveGeneric, FlexibleContexts,
             FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, NamedFieldPuns,
             NoImplicitPrelude, OverloadedStrings, RankNTypes, ScopedTypeVariables,
             StandaloneDeriving, TemplateHaskell, TupleSections #-}
{-# OPTIONS_GHC -funbox-strict-fields -fno-warn-orphans -Werror #-}

module FP.Store.Blob where

import qualified Aws
import qualified Aws.S3                    as Aws
import           ClassyPrelude.Yesod
import           Control.Retry             (retrying)
import           Data.Aeson                ((.:?), (.!=))
import           Data.Conduit.Binary       (sourceFileRange)
import qualified Data.List
import qualified Data.Map                  as Map
import           Data.MIME.Types           (guessType, defaultmtd)
import qualified Data.Set                  as Set
import           Data.Yaml                 (decodeEither)
import           Filesystem                (createTree, isDirectory, listDirectory,
                                            removeDirectory, removeFile)
import qualified Filesystem.Path.CurrentOS as FP
import qualified Network.HTTP.Conduit      as NHC

import qualified Azure.BlobStorage         as Azure
import           FP.EnvSettings            (awsAccessKey, awsSecretKey, azureAccountKey,
                                            azureAccountName, azureBlobStorageUrl,
                                            blobStoreSettingsPath)

data Bucket = FpcoDataBucket
    deriving (Show, Eq, Read, Enum, Bounded, Ord)

bucketName :: Bucket -> Text
bucketName bucket = case bucket of
    FpcoDataBucket   -> "fpco-data"

bucketNameMap :: Map Text Bucket
bucketNameMap = concatMap (\x -> singletonMap (bucketName x) x) [minBound..maxBound]

data BlobName = Media MediaUUIDType
    deriving (Show, Eq, Read, Generic)
instance ToJSON BlobName
instance FromJSON BlobName

nameToBucketPath :: BlobName -> (Bucket, Text)
nameToBucketPath blobName = case blobName of
    Media x -> (FpcoDataBucket, "media/" ++ toPathPiece x)

data MediaType = PNG | GIF | JPEG
    deriving (Show, Eq, Read, Enum, Bounded, Generic)
instance ToJSON MediaType
instance FromJSON MediaType

mediaTypeExt :: MediaType -> Text
mediaTypeExt PNG = "png"
mediaTypeExt GIF = "gif"
mediaTypeExt JPEG = "jpeg"

mediaTypeMime :: MediaType -> ContentType
mediaTypeMime PNG = "image/png"
mediaTypeMime GIF = "image/gif"
mediaTypeMime JPEG = "image/jpeg"

mtypeExtMap :: Map Text MediaType
mtypeExtMap = concatMap (\x -> singletonMap (mediaTypeExt x) x) [minBound..maxBound]

mtypeMimeMap :: Map ContentType MediaType
mtypeMimeMap = concatMap (\x -> singletonMap (mediaTypeMime x) x) [minBound..maxBound]

-- | UUID for media lookups in blob storage
newtype MediaUUID = MediaUUID Text
    deriving (Show, Eq, Read, Ord, Generic, Typeable, ToJSON, FromJSON,
              PersistField, PathPiece, ToMessage)

instance PathPiece MediaUUIDType where
    toPathPiece (MediaUUIDType (MediaUUID uuid) mtype) = concat [uuid, ".", mediaTypeExt mtype]
    fromPathPiece t = do
        let (x, y) = break (== '.') t
        uuid <- fromPathPiece x
        z <- stripPrefix "." y
        mtype <- lookup z mtypeExtMap
        return $ MediaUUIDType uuid mtype

data MediaUUIDType = MediaUUIDType !MediaUUID !MediaType
    deriving (Show, Eq, Read, Generic)
instance ToJSON MediaUUIDType
instance FromJSON MediaUUIDType

data Backend = Backend
    { backendGet    :: Text -> Maybe (Int64, Int64) -> ResourceT IO (Maybe (ResumableSource (ResourceT IO) ByteString))
    , backendPut    :: Text -> Source (ResourceT IO) ByteString -> Int64 -> ResourceT IO ()
    , backendDelete :: Text -> ResourceT IO ()
    , backendList   :: Text -> ResourceT IO [Text]
    , backendUrl    :: Text -> IO Text
    }

type GetMakeBackend = IO (Manager -> Backend)

s3Backend :: Text -> Text -> Maybe Aws.CannedAcl -> Text -> Bool -> GetMakeBackend
s3Backend bucket prefix maybeAcl urlPrefix guessMimeType = do
    ref <- newIORef []
    let creds = Aws.Credentials awsAccessKey awsSecretKey ref Nothing
        config = (Aws.Configuration Aws.Timestamp creds $ Aws.defaultLog Aws.Error)
    return $ \manager -> Backend
        { backendGet = \path mrange -> do
            let req = (Aws.getObject bucket (prefix ++ path))
                    { Aws.goResponseContentRange = bimap fromIntegral fromIntegral <$> mrange }
            res <- awsRetry config Aws.defServiceConfig manager req
            egor <- tryJust (\(e::SomeException) ->
                        if "The specified key does not exist" `isInfixOf` tshow e
                            then Just ()
                            else Nothing) $
                Aws.readResponseIO res
            case egor of
                Left _ -> return Nothing
                Right gor -> return (Just $ responseBody $ Aws.gorResponse gor)
        , backendPut = \path src size -> do
            let req = (Aws.putObject bucket (prefix ++ path) (NHC.requestBodySource size src))
                        { Aws.poAcl = maybeAcl
                        , Aws.poContentType = if guessMimeType
                            then (encodeUtf8 . pack) <$> (fst $ guessType defaultmtd False $ unpack path)
                            else Nothing }
            res <- awsRetry config Aws.debugServiceConfig manager req
            _ <- Aws.readResponseIO res
            return ()
        , backendDelete = \path -> do
            let req = Aws.DeleteObject (prefix ++ path) bucket
            res <- awsRetry config Aws.debugServiceConfig manager req
            _ <- Aws.readResponseIO res
            return ()
        , backendList = \listPrefix ->
            map (drop (length prefix + length listPrefix))
                <$> makeListRequest (prefix ++ listPrefix) manager config Nothing True
        , backendUrl = \path ->
            return $ concat
                [ urlPrefix
                , bucket
                , "/"
                , prefix
                , path
                ]
        }
  where
    makeListRequest _ _ _ _ False = return []
    makeListRequest listPrefix manager config mmarker True =  do
        let req = (Aws.getBucket bucket)
                     { Aws.gbPrefix = Just listPrefix
                     , Aws.gbMarker = mmarker
                     }
        res <- awsRetry config Aws.defServiceConfig manager req
        gbr <- Aws.readResponseIO res
        let contents = map Aws.objectKey (Aws.gbrContents gbr)
        case contents of
            [] -> return []
            _  -> (++) <$> pure contents
                       <*> makeListRequest listPrefix manager config (Just (Data.List.last contents))
                               (Aws.gbrIsTruncated gbr)
    awsRetry cfg svcfg mgr r =
        liftResourceT $ retrying def (const $ return . isLeft . Aws.responseResult) $
            Aws.aws cfg svcfg mgr r
      where
        isLeft Left{} = True
        isLeft Right{} = False

azureBackend :: Text -> Text -> GetMakeBackend
azureBackend containerId' prefix = do
    let containerId = Azure.ContainerID containerId'
    bs <- Azure.blobStorageFromUrl
        (unpack azureBlobStorageUrl)
        (Azure.AccountName azureAccountName)
        (Azure.parseSharedKeyOrFail azureAccountKey)
    return $ \_manager ->
        Backend { backendGet = \path mrange -> do
            er <- tryJust (\e -> case e of
                    Azure.BlobNotFound -> Just e
                    _ -> Nothing) $
                Azure.getBlob bs containerId (toBlobId path) mrange
            case er of
                Left _ -> return Nothing
                Right r -> return (Just r)
            , backendPut = \path src _size ->
                Azure.putBlob bs containerId (toBlobId path) src
            , backendDelete = \path -> liftIO $
                Azure.deleteBlob bs containerId (toBlobId path)
            , backendList = \listPrefix -> liftIO $
                map (\(Azure.BlobID bid) -> drop (length prefix + length listPrefix) bid)
                    <$> Azure.listBlobs bs containerId (toBlobId listPrefix) 5000
            , backendUrl = \path ->
                return $ concat
                    [ azureBlobStorageUrl
                    , containerId'
                    , "/"
                    , prefix
                    , path
                    ]
            }
  where
    toBlobId path = Azure.BlobID (prefix ++ path)

fsBackend :: FilePath -> GetMakeBackend
fsBackend root' = do
    let root = FP.collapse root'
        mkfp :: Monad m => Text -> m FP.FilePath
        mkfp path = do
            let fp = root </> fpFromText path
                cfp = FP.collapse fp
            when (cfp /= fp) $ error $ "fsBackend: path resolves outside of root: " ++ show path
            return cfp
    return $ \_manager -> Backend
        { backendGet = \path mrange -> do
            fp <- mkfp path
            es <- try $ fst <$> (sourceFileRange (fpToString fp)
                    ((fromIntegral . fst) <$> mrange) ((\(b,e) -> fromIntegral (b-e+1)) <$> mrange)
                $$+ return ())
            case es of
                Left e -> if isDoesNotExistError e
                    then return Nothing
                    else throwIO e
                Right s -> return (Just s)
        , backendPut = \path src _len -> do
            fp <- mkfp path
            lift $ createTree (directory fp)
            src $$ sinkFile fp
        , backendDelete = \path -> liftIO $ do
            fp <- mkfp path
            removeFile fp
            removeDirectoryRecursive root (directory fp)
        , backendList = \prefix -> liftIO $ do
            fp <- mkfp prefix
            map (fpToText . fromMaybe FP.empty . FP.stripPrefix fp) <$> listDirectoryRecursive fp
        , backendUrl = \path -> do
            fp <- mkfp path
            return $ "file://" ++ fpToText fp
        }
  where
    listDirectoryRecursive fp = do
        fps <- listDirectory fp
        result <- forM fps $ \fp' -> do
            isDir <- isDirectory fp'
            if isDir
                then listDirectoryRecursive fp'
                else return [fp']
        return (concat result)
    removeDirectoryRecursive root fp =
        handle (\(_::IOError) -> return ()) $ do
            removeDirectory fp
            let pfp = FP.parent fp
            when (FP.commonPrefix [root, pfp] /= root) $
                removeDirectoryRecursive root pfp


instance FromJSON GetMakeBackend where
    parseJSON (Object o)
        =   (s3Backend
                <$> o .: "s3-bucket"
                <*> o .:? "s3-prefix" .!= ""
                <*> o .:? "s3-acl"
                <*> o .:? "s3-url-prefix" .!= "https://s3.amazonaws.com/"
                <*> o .:? "s3-guess-mime-type" .!= False)
        <|> (azureBackend
                <$> o .: "azure-container"
                <*> o .:? "azure-prefix" .!= "")
        <|> (fsBackend
                <$> (fpFromText <$> o .: "fs-path"))
    parseJSON _ = fail $ "Expected object"

instance FromJSON Aws.CannedAcl where
    parseJSON (String s) = case s of
        "private" -> pure Aws.AclPrivate
        "public-read" -> pure Aws.AclPublicRead
        "public-read-write" -> pure Aws.AclPublicReadWrite
        "authenticated-read" -> pure Aws.AclAuthenticatedRead
        "bucket-owner-read" -> pure Aws.AclBucketOwnerRead
        "bucket-owner-full-control" -> pure Aws.AclBucketOwnerFullControl
        "log-delivery-write" -> pure Aws.AclLogDeliveryWrite
        _ -> fail $ "Invalid S3 ACL"
    parseJSON _ = fail $ "Expected string"

data Settings = Settings
    { settingsBucketGetMakeBackends :: Map Bucket GetMakeBackend
    }

instance FromJSON Settings where
    parseJSON (Object o) = Settings
        <$> (Map.mapKeys (\x -> fromMaybe (error $ "Blob store: unknown bucket name: " ++ x) $
                            lookup (pack x) bucketNameMap) <$> o .: "buckets")
    parseJSON _ = fail "Expected object"

data Store = Store
    { storeBucketBackends :: Map Bucket Backend
    }

getMakeStoreWithSettings :: FilePath -> IO (Manager -> Store)
getMakeStoreWithSettings settingsPath = do
    bs <- readFile settingsPath
    case decodeEither bs of
        Left s -> error $ "Could not read: " ++ show settingsPath ++ ": " ++ s
        Right r -> do
            let missingBuckets = Map.keysSet (settingsBucketGetMakeBackends r)
                    `Set.difference` Set.fromList [minBound .. maxBound]
            when (not $ Set.null missingBuckets) $
                error $ "Blob store: missing bucket(s) in config: " ++ show (Set.toList missingBuckets)
            r' <- forM (Map.toList $ settingsBucketGetMakeBackends r) $ \(k, c) -> do
                makeBackend <- c
                return (k, makeBackend)
            return $ \mgr -> Store $ Map.fromList $ map (\(k,b) -> (k, b mgr)) r'

getMakeStore :: IO (Manager -> Store)
getMakeStore = getMakeStoreWithSettings $ fpFromText blobStoreSettingsPath

getBackend :: Store -> Bucket -> Backend
getBackend Store{storeBucketBackends} bucket =
    fromMaybe (error $ "Blob store: couldn't find bucket in config: " ++ show bucket) $ lookup bucket storeBucketBackends

withBackend :: (MonadIO m) => Store -> BlobName -> (Backend -> Text -> m a) -> m a
withBackend store blobName action = do
    let (bucket, path) = nameToBucketPath blobName
    action (getBackend store bucket) path

maybeGetWithRange :: Store -> BlobName -> Maybe (Int64, Int64) -> ResourceT IO (Maybe (ResumableSource (ResourceT IO) ByteString))
maybeGetWithRange store blobName mrange = withBackend store blobName $ \backend path ->
    (backendGet backend) path mrange

maybeGet :: Store -> BlobName -> ResourceT IO (Maybe (ResumableSource (ResourceT IO) ByteString))
maybeGet store blobName = maybeGetWithRange store blobName Nothing

get :: Store -> BlobName -> ResourceT IO (ResumableSource (ResourceT IO) ByteString)
get store blobName = do
    ms <- maybeGet store blobName
    case ms of
        Nothing -> error ("Blob store: blob not found: " ++ show blobName)
        Just s -> return s

delete :: Store -> BlobName -> ResourceT IO ()
delete store blobName = withBackend store blobName $ \backend path ->
    (backendDelete backend) path

put :: Store -> BlobName -> Source (ResourceT IO) ByteString -> Int64 -> ResourceT IO ()
put store blobName src size = withBackend store blobName $ \backend path ->
    (backendPut backend) path src size

list :: Store -> BlobName -> ResourceT IO [Text]
list store blobName = withBackend store blobName $ \backend path ->
    (backendList backend) path

url :: Store -> BlobName -> IO Text
url store blobName = withBackend store blobName $ \backend path ->
    (backendUrl backend) path
