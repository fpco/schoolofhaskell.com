{-# OPTIONS_GHC -fno-warn-orphans -O1 #-}
module Application
    ( main
    , develMain
    , makeFoundation
    ) where

import Import hiding (Proxy)
import Yesod.Default.Main
import Yesod.Default.Handlers hiding (getRobotsR)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.MethodOverride
import Network.Wai.Application.Static (defaultFileServerSettings)
import System.Directory (doesFileExist)
import System.Exit (exitWith, ExitCode (ExitSuccess))
import FP.EnvSettings
import qualified FP.Store.Blob as Blob
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setPort, setHost, setOnException, defaultShouldDisplayException)
import Language.Haskell.TH.Syntax (qLocation)
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
import Import.Migration
import Database.Persist.Sql (migrate)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.UserTutorial
import Handler.User
import Handler.UserFeed
import Handler.BuildVersion
import Handler.Media
import Handler.Export
import Handler.Users
import Handler.ContentProxy
import Handler.RecentContent
import Handler.RecentContentFeed
import Handler.RecentContentFeedEntry
import Handler.Sitemap
import Handler.TutorialRaw
import Handler.Search

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
                -> IO (Application, LogFunc)
makeApplication ghid ghsecret = do
    (foundation, logFunc) <- makeFoundation ghid ghsecret

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
             in return $ basicAuth check' "School of Haskell"

makeFoundation :: ByteString -- ^ github client id
               -> ByteString -- ^ github client secret
               -> IO (App, LogFunc)
makeFoundation ghid ghsecret = do
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
    exportStatus <- newIORef ExportNotComplete

    let app = App
            { getStatic = s
            , getWPStatic = Static $ defaultFileServerSettings "content/wp-content"
            , connPool = p
            , httpManager = manager
            , persistConfig = dbconf
            , appBlobStore = makeBlobStore manager
            , githubClientId = ghid
            , githubClientSecret = ghsecret
            -- , ekgVariables = (counters,gauges,labels)
            , staticRoot = fromMaybe (learningSiteApproot ++ "/static") learningSiteStaticRoot
            , appLogger = logger
            , appPrivate = learningSitePrivate
            , appRandom = randomGen
            , appGhcEnvs = ghcEnvs
            , appClientSessionBackend = defClientSessionBackend
            , googleOAuth = (googleOAuthClientId, googleOAuthClientSecret)
            , appAdmins = getAdmins
            , appExportStatus = exportStatus
            }

    -- Migrations disabled for SoH until FPHC is shut down. Use
    -- LEARNING_SITE_DO_MIGRATION=YES to re-enable. Particularly useful
    -- to initialize an empty database.
    when shouldDoMigration
        $ runResourceT
        $ flip runLoggingT (messageLoggerSource app logger)
        $ runSqlPool p $ do
            let ms = MigrationSettings
            let mpDef = entityDef (Nothing :: Maybe MigrationPerformed)
            runMigration $ migrate [mpDef] mpDef
            performPreMigrations ms
            runMigration migrateAll
            performPostMigrations ms app

    let processPeriod = 300
        periodically source f = void $ forkIO $ flip runLoggingT (messageLoggerSource app logger) $ do
            handleAny ($logErrorS source . tshow) f
            liftIO $ threadDelay (1000 * 1000 * processPeriod)

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

    void $ forkIO $ performExportLoop (appExportStatus app) p

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
    makeApplication ghid ghsecret
  where
    -- TODO: need something here once github is used for something.
    ghid = "FIXME"
    ghsecret = "FIXME"

main :: IO ()
main = useSyslog "soh-site" $ do
    makeApp <-
        if deploymentType == DevDeployment
            then return makeApplicationDev
            else return $ makeApplication
                    (encodeUtf8 githubClientID)
                    (encodeUtf8 FP.EnvSettings.githubClientSecret)
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
