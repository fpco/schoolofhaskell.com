{-# OPTIONS_GHC -fno-warn-orphans -O1 #-}
module Application
    ( main
    , develMain
    , makeFoundation
    ) where

import Import hiding (Proxy)
import Yesod.Auth
import Yesod.Default.Main
import Yesod.Default.Handlers hiding (getRobotsR)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.MethodOverride
import Network.Wai.Application.Static (defaultFileServerSettings)
import System.Directory (doesFileExist)
import System.Exit (exitWith, ExitCode (ExitSuccess))
import System.Process (rawSystem)
import FP.EnvSettings
import qualified FP.Store.Blob as Blob
import Network.Mail.Mime (Address (Address), Mail, renderMail', renderSendMail)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setPort, setHost, setOnException, defaultShouldDisplayException)
import Language.Haskell.TH.Syntax (qLocation)
import Network.Mail.Mime.SES (renderSendMailSES, SES (SES), usEast1)
import Network.Wai.Middleware.Gzip (gzip)
import System.Log.FastLogger
import Control.Concurrent (threadDelay)
import Control.Monad.Logger (runLoggingT, liftLoc)
import FP.Logger (syslogMessageLogger)
import Network.Wai.Middleware.HttpAuth (basicAuth)
import System.Random.MWC (withSystemRandom, uniformVector, initialize)
import Control.Concurrent (forkIO)
import Data.Time (addUTCTime)
import FP.Logger (useSyslog)
import Database.Persist.Postgresql (PostgresConf (..))
import Network.Wai.Middleware.AcceptOverride
import Network.Wai.Logger (clockDateCacher)
import qualified Yesod.Core.Types
import Database.Persist.Timed.Postgresql (createPidConnectionPool)
import System.Environment (getEnvironment)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Dashboard
import Handler.Profile
import Handler.ResetSecurityToken
import Handler.VerifyEmail
import Handler.UserTutorial
import Handler.User
import Handler.UserFeed
import Handler.TutorialPreview
import Handler.BuildVersion
import Handler.Media
import Handler.MediaUpload
import Handler.Users
import Handler.ContentProxy
import Handler.RecentContent
import Handler.RecentContentFeed
import Handler.RecentContentFeedEntry
import Handler.Sitemap
import Handler.Github
import Handler.GithubCallback
import Handler.TutorialRaw
import Handler.Search
import Handler.AuthInfo
import Handler.AddEmail
import Handler.ConfirmEmail
import Handler.MakePrimaryEmail
import Handler.DeleteEmail
import Handler.SetSkillLevel
import Handler.EditTutorial
import Handler.Arrange
import Handler.NewGroup
import Handler.NewTutorial
import Handler.ImportContent
import Handler.EditGroup
import Handler.DeleteMember
import Handler.LikeContent
import Handler.ExportAllTutorials
import Handler.Admin
import Handler.AdminActions
import Handler.AdminLoginAnalytics
import Handler.AdminSystem
import Handler.AdminUserList
import Handler.AdminVerificationUrl
import Handler.ImpersonateUser
import Handler.NewSshKey

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: ByteString -- ^ github client id
                -> ByteString -- ^ github client secret
                -> (Manager -> Text -> (Text -> Mail) -> ResourceT IO ()) -- ^ render and send email
                -> IO (Application, LogFunc)
makeApplication ghid ghsecret rsm = do
    (foundation, logFunc) <- makeFoundation ghid ghsecret rsm

    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromHeader
        , destination = Logger $ Yesod.Core.Types.loggerSet $ appLogger foundation
        }

    authMiddleware <- getAuthMiddleware

    app <- toWaiAppPlain foundation
    return
        ( gzip def
        $ authMiddleware
        $ methodOverride
        $ acceptOverride
        $ logWare app
        , logFunc)

-- | Apply HTTP basic authentication requirements based on the HTTP_USERNAME
-- and HTTP_PASSWORD environment variables.
getAuthMiddleware :: IO Middleware
getAuthMiddleware =
    case authMiddlewareUsernamePassword of
        Nothing -> return id
        Just (u1, p1) ->
            let check' u2 p2 = return $ u1 == u2 && p1 == p2
             in return $ basicAuth check' "FP Complete Haskell Center"

makeFoundation :: ByteString -- ^ github client id
               -> ByteString -- ^ github client secret
               -> (Manager -> Text -> (Text -> Mail) -> ResourceT IO ()) -- ^ render and send email
               -> IO (App, LogFunc)
makeFoundation ghid ghsecret rsm = do
    manager <- newManager

    s <- staticSite

    let dbconf = PostgresConf learningSiteConnStr learningSitePoolSize

    let logFunc = syslogMessageLogger (\a b -> return $ shouldLog' a b)

    p <- createPidConnectionPool logFunc (pgConnStr dbconf) (pgPoolSize dbconf)

    loggerSet <- newStdoutLoggerSet defaultBufSize
    (getter, _) <- clockDateCacher
    let logger = Yesod.Core.Types.Logger loggerSet getter

    let asIO :: IO a -> IO a
        asIO = id
    randomGen <- withSystemRandom $ \gen -> uniformVector gen 200 >>=
                                            asIO . initialize . asVector

    let ghcEnvs = []

    defClientSessionBackend <- defaultClientSessionBackend sessionDuration "config/client_session_key.aes"

    makeBlobStore <- Blob.getMakeStore

    let app = App
            { getStatic = s
            , getWPStatic = Static $ defaultFileServerSettings "content/wp-content"
            , connPool = p
            , httpManager = manager
            , persistConfig = dbconf
            , appBlobStore = makeBlobStore manager
            , githubClientId = ghid
            , githubClientSecret = ghsecret
            , appRenderSendMail = \manager' text withFrom ->
                let withFrom' from = withFrom $ Address (Just "FP Complete") from
                 in rsm manager' text withFrom'
            -- , ekgVariables = (counters,gauges,labels)
            , staticRoot = fromMaybe (learningSiteApproot ++ "/static") learningSiteStaticRoot
            , appLogger = logger
            , appPrivate = learningSitePrivate
            , appRandom = randomGen
            , appGhcEnvs = ghcEnvs
            , appClientSessionBackend = defClientSessionBackend
            , googleOAuth = (googleOAuthClientId, googleOAuthClientSecret)
            }

    -- Migrations disabled for SoH until FPHC is shut down

    -- runResourceT
    --      $ flip runLoggingT (messageLoggerSource app logger)
    --      $ runSqlPool p
    --      $ Redis.withMutex redisConn "fpco.learning-site.migrate.mutex" $ do
    --             let ms = MigrationSettings
    --             let mpDef = entityDef (Nothing :: Maybe MigrationPerformed)
    --             runMigration $ migrate [mpDef] mpDef
    --             performPreMigrations ms
    --             runMigration migrateAll
    --             performPostMigrations ms app

    let processPeriod = 300
        periodically source f = void $ forkIO $ flip runLoggingT (messageLoggerSource app logger) $ do
            liftIO $ threadDelay (1000 * 1000 * processPeriod)
            handleAny ($logErrorS source . tshow) f

    periodically "PRUNING"
        $ runResourceT
        $ pruneTutorials p

    periodically "PRUNING-SERVER-SESSION"
          $ runResourceT
          $ runSqlPool p
          $ do
              now' <- liftIO getCurrentTime

              -- Delete server sessions that are one hour old
              deleteCascadeWhere [ServerSessionWhen <. addUTCTime (-3600) now']

              -- Delete any records in the session log more than five minutes old
              deleteWhere [SessionLogWhen <. addUTCTime (-300) now']

              -- Delete UserDeletion records that are one hour old.
              deleteWhere [UserDeletionCreated <. addUTCTime (-3600) now']

    return (app, logFunc)

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev = do
    env <- getEnvironment
    p <-
        case lookup "PORT" env of
            Nothing -> error "yesod devel did not set PORT environment variable"
            Just str ->
                case readMay str of
                    Nothing -> error $ "Invalid PORT environment variable: " ++ str
                    Just p -> return p
    let pdisplay = fromMaybe p $ lookup "DISPLAY_PORT" env >>= readMay
    putStrLn $ "Devel application launched: http://localhost:" ++ tshow pdisplay
    (app, _logFunc) <- makeApplicationDev
    return (p, app)

makeApplicationDev :: IO (Application, LogFunc)
makeApplicationDev =
    makeApplication ghid ghsecret devrsm
  where
    -- TODO: need something here once github is used for something.
    ghid = "FIXME"
    ghsecret = "FIXME"

-- | Use the system sendmail command to render and send email.
sendmailRSM :: MonadIO m
            => Text
            -> Manager
            -> Text
            -> (Text -> Mail)
            -> m ()
sendmailRSM from _manager _email mkmail = liftIO $ renderSendMail $ mkmail from

devrsm :: MonadIO m
       => Manager
       -> Text
       -> (Text -> Mail)
       -> m ()
devrsm _manager _email mail = liftIO $ do
    bs <- renderMail' $ mail "noreply@fpcomplete.com"
    writeFile "/tmp/mail.txt" bs
    void $ rawSystem "xdg-open" ["/tmp/mail.txt"]

main :: IO ()
main = useSyslog "soh-site" $ do
    makeApp <-
        if deploymentType == DevDeployment
            then return makeApplicationDev
            else do
                let sesFrom = learningSiteMailFrom
                let rsmAwsSes manager email mkmail = renderSendMailSES
                        manager
                        (SES (encodeUtf8 sesFrom) [encodeUtf8 email] awsAccessKey awsSecretKey usEast1)
                        (mkmail sesFrom)
                    rsm
                        | deploymentType == AwsDeployment = rsmAwsSes
                        | development = devrsm
                        | otherwise   = sendmailRSM sesFrom

                return $ makeApplication
                    (encodeUtf8 githubClientID)
                    (encodeUtf8 FP.EnvSettings.githubClientSecret)
                    rsm
    (app, logFunc) <- makeApp
    let onExc _ e =  when (defaultShouldDisplayException e) $ logFunc
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e)
    flip runSettings app
        $ setPort learningSitePort
        $ setHost learningSiteHost
        $ setOnException onExc
          defaultSettings

develMain :: IO ()
develMain = useSyslog "soh-site" $ do
    putStrLn "Starting devel application"
    (port, app) <- getApplicationDev
    _ <- forkIO $ runSettings (setPort port defaultSettings) app
    let loop = do
            threadDelay 100000
            e <- doesFileExist "dist/devel-terminate"
            if e
                then terminateDevel
                else loop
    loop

terminateDevel :: IO ()
terminateDevel = exitWith ExitSuccess
