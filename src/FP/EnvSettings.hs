{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Load settings from environment variables.
module FP.EnvSettings where

import ClassyPrelude
import Control.Exception (throw)
import Control.Monad.Logger (LogLevel(..), LogSource)
import Data.Streaming.Network (HostPreference)
import System.Environment (getEnvironment, getProgName)
import System.IO.Unsafe (unsafePerformIO)

progName :: Text
progName = pack $ unsafePerformIO $ getProgName
{-# NOINLINE progName #-}

addProgName :: String -> String
addProgName s
    | isProductionServer0 = s
    | otherwise = concat
        [ "["
        , unpack progName
        , "] "
        , s
        ]
  where
    -- Avoid an infinite loop #3661
    isProductionServer0 :: Bool
    isProductionServer0 = lookup "FP_ENVIRONMENT_NAME" initialEnv0 == Just "production"

data EnvException
    = EnvVarNotSet Text
    | InvalidEnvVar Text Text
    | InvalidServerInfo Text Text
  deriving Typeable
instance Exception EnvException
instance Show EnvException where
    show (EnvVarNotSet key) = addProgName $ "Environment variable not set: " ++ unpack key
    show (InvalidEnvVar key val) = addProgName $ concat
        [ "Invalid "
        , unpack key
        , " environment variable: "
        , unpack val
        ]
    show (InvalidServerInfo key value) = addProgName $ concat
        [ "Invalid server info in "
        , unpack key
        , ": "
        , unpack value
        ]

initialEnv0 :: HashMap Text Text
initialEnv0 = mapFromList $ map (pack *** pack) $ unsafePerformIO getEnvironment
{-# NOINLINE initialEnv0 #-}

initialEnv :: HashMap Text Text
initialEnv
    | lookup "FP_ENVIRONMENT_NAME" initialEnv0 == Just "development" =
        initialEnv0 ++ develDefaults
    | otherwise = initialEnv0
  where
    isTest = progName == "test"

    develDefaults = mapFromList
        [ ("FP_ENVIRONMENT_TYPE", "development")
        , ("FP_DEPLOYMENT_TYPE", "development")
        , ("BLOB_STORE_SETTINGS", "config/blob-store.yaml")
        , ("LEARNING_SITE_PGCONNSTR",
            if isTest
                then "dbname=learning_site_test host=localhost user=learning_site password=learning-site port=5432"
                else "dbname=learning_site host=localhost user=learning_site password=learning-site port=5432")
        , ("LEARNING_SITE_PGPOOLSIZE", "7")
        , ("LEARNING_SITE_MAIL_FROM", "noreply@fpcomplete.com")
        , ("LEARNING_SITE_ANALYTICS", "UA-36928035-1")
        , ("LEARNING_SITE_BIND", "*4")
        , ("LEARNING_SITE_PORT", "3000")
        , ("LEARNING_SITE_APPROOT", "http://localhost:3000")
        , ("GOOGLE_OAUTH_CLIENT_ID", "219123459670-s2a0rs5tjptqol2oo3f0u27ac6it9riv.apps.googleusercontent.com")
        , ("GOOGLE_OAUTH_CLIENT_SECRET", "VPosg0Tt04Bwv8Hj-U3L6fhN")
        ]

lookupEnv0 :: MonadThrow m => Text -> m Text
lookupEnv0 k = maybe (throwM $ EnvVarNotSet k) return $ lookup k initialEnv0

lookupEnv :: MonadThrow m => Text -> m Text
lookupEnv k = maybe (throwM $ EnvVarNotSet k) return $ lookup k initialEnv

lookupEnvError :: Text -> Text
lookupEnvError = either throw id . lookupEnv

lookupReadEnv :: (MonadThrow m, Read a) => Text -> m a
lookupReadEnv k =
    lookupEnv k >>= readMay'
  where
    readMay' v =
        case readMay v of
            Nothing -> throwM $ InvalidEnvVar k v
            Just v' -> return v'

lookupReadEnvError :: Read a => Text -> a
lookupReadEnvError = either throw id . lookupReadEnv

fpEnvironmentNameMaybe :: MonadThrow m => m Text
fpEnvironmentNameMaybe = lookupEnv0 "FP_ENVIRONMENT_NAME"

fpEnvironmentName :: Text
fpEnvironmentName = fromMaybe "development" fpEnvironmentNameMaybe

fpEnvironmentTypeMaybe :: MonadThrow m => m Text
fpEnvironmentTypeMaybe = maybe fpEnvironmentNameMaybe return
                       $ lookupEnv "FP_ENVIRONMENT_TYPE"

fpEnvironmentType :: Text
fpEnvironmentType = either throw id fpEnvironmentTypeMaybe

-- | Returns 'True' when we're running on either the \"staging\" or
--   \"production\" server.
isProductionOrStaging :: Bool
isProductionOrStaging = fpEnvironmentType == "production" || fpEnvironmentType == "staging"

-- | Returns 'True' when we're running on the \"production\" server.
isProductionServer :: Bool
isProductionServer = fpEnvironmentType == "production"

data DeploymentType = DevDeployment
                    | StandaloneDeployment
                    | AwsDeployment
                    | AzureDeployment
    deriving (Read, Show, Eq)

deploymentType :: DeploymentType
deploymentType = case lookupEnvError "FP_DEPLOYMENT_TYPE" of
    "development" -> DevDeployment
    "standalone" -> StandaloneDeployment
    "aws" -> AwsDeployment
    "azure" -> AzureDeployment
    _ -> error $ "Invalid FP_DEPLOYMENT_TYPE"

standaloneOrDev :: Bool
standaloneOrDev = deploymentType `elem` [StandaloneDeployment, DevDeployment]

standalone :: Bool
standalone = deploymentType `elem` [StandaloneDeployment]

-- | Turn on verbose debugging?
verboseDebugging :: Bool
verboseDebugging = lookupEnv "VERBOSE_DEBUGGING" == Just "1"

-- | For use with monad-logger, use the verboseDebugging setting.
shouldLogDebugIO :: LogSource -> LogLevel -> IO Bool
shouldLogDebugIO _src lev =
    return (lev >= (if verboseDebugging
                        then LevelDebug
                        else LevelInfo))

fpLogStderr :: Bool
fpLogStderr = lookupEnv "FP_LOG_NOSTDERR" /= Just "1"

-- awsInstanceID :: Maybe Text
-- awsInstanceID = lookupEnv "AWS_INSTANCE_ID"

-- | The initial environment variables, cleaned up to remove any variables that
-- the user shouldn't see. This is usable in the context of isolation-runner
-- calling user code.
userSafeEnvironment :: HashMap Text Text
userSafeEnvironment =
    mapFromList $ filter ((`member` allowed) . fst) $ mapToList initialEnv
  where
    allowed = asHashSet $ setFromList $ words
        "PATH TERM RUNLEVEL PWD LANG TZ SHLVL HOME USER"

applyUserSafeEnvironment :: [(String, String)] -> [(String, String)]
applyUserSafeEnvironment =
     map (unpack *** unpack) .
     mapToList .
     (<> userSafeEnvironment) .
     mapFromList .
     map (pack *** pack)

externalPort :: Int
externalPort = lookupReadEnvError "EXTERNAL_PORT"

awsAccessKey :: ByteString
awsAccessKey = encodeUtf8 $ lookupEnvError "AWS_ACCESS_KEY"

awsSecretKey :: ByteString
awsSecretKey = encodeUtf8 $ lookupEnvError "AWS_SECRET_KEY"

azureBlobStorageUrl :: Text
azureBlobStorageUrl = lookupEnvError "AZURE_BLOB_STORAGE_URL"

azureAccountName :: ByteString
azureAccountName = encodeUtf8 $ lookupEnvError "AZURE_ACCOUNT_NAME"

azureAccountKey :: ByteString
azureAccountKey = encodeUtf8 $ lookupEnvError "AZURE_ACCOUNT_KEY"

authMiddlewareUsernamePassword :: Maybe (ByteString, ByteString)
authMiddlewareUsernamePassword = (,)
    <$> (encodeUtf8 <$> lookupEnv "HTTP_USERNAME")
    <*> (encodeUtf8 <$> lookupEnv "HTTP_PASSWORD")

learningSiteMailFrom :: Text
learningSiteMailFrom = lookupEnvError "LEARNING_SITE_MAIL_FROM"

learningSiteStaticRoot :: Maybe Text
learningSiteStaticRoot = lookupEnv "STATIC_ROOT"

learningSitePrivate :: Bool
learningSitePrivate = lookupEnv "PRIVATE" == Just "TRUE"

githubClientID, githubClientSecret :: Text
githubClientID = lookupEnvError "GITHUB_CLIENT_ID"
githubClientSecret = lookupEnvError "GITHUB_CLIENT_SECRET"

blobStoreSettingsPath :: Text
blobStoreSettingsPath = lookupEnvError "BLOB_STORE_SETTINGS"

learningSiteConnStr :: ByteString
learningSiteConnStr = encodeUtf8 $ lookupEnvError "LEARNING_SITE_PGCONNSTR"

learningSitePoolSize :: Int
learningSitePoolSize = lookupReadEnvError "LEARNING_SITE_PGPOOLSIZE"

learningSiteAnalytics :: Maybe Text
learningSiteAnalytics
    | null x = Nothing
    | otherwise = Just x
  where
    x = lookupEnvError "LEARNING_SITE_ANALYTICS"

learningSiteHost :: HostPreference
learningSiteHost = fromString $ unpack $ lookupEnvError "LEARNING_SITE_BIND"

learningSitePort :: Int
learningSitePort = lookupReadEnvError "LEARNING_SITE_PORT"

learningSiteApproot :: Text
learningSiteApproot = lookupEnvError "LEARNING_SITE_APPROOT"

googleOAuthClientId :: Text
googleOAuthClientId = lookupEnvError "GOOGLE_OAUTH_CLIENT_ID"

googleOAuthClientSecret :: Text
googleOAuthClientSecret = lookupEnvError "GOOGLE_OAUTH_CLIENT_SECRET"
