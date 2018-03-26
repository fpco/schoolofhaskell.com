{-# OPTIONS -fno-warn-orphans -O1 #-}
module Foundation
    ( App (..)
    , Route (..)
    , resourcesApp
    , Handler
    , Widget
    , Form
    , maybeAuth
    , maybeAuthId
    , requireAuth
    , requireAuthId
    , requireAuthMsg
    , requireProfile
    , module Settings
    , module Model
    , defaultLayoutExtra
    , prettyProfile
    , defaultWidget
    , defaultWidgetJs
    , getNotFoundPage
    , isAdmin
    , maybeServerSession
    , requireServerSession
    , setServerSessionIfMissing
    , getServerSessionKey
    , sessionLog
    , sessionLogDB
    , newSecurityToken
    , makeGoogleAnalytics
    , makeGAPath
    , checkPasswordSecurity'
    , Import.DBTimed.runDB
    , getUserByEmail
    , getUserByEmail404
    , getClearServerSessionR
    , sendVerifyEmailHelper
    , Foundation.randomString
    , addUnverifiedDB
    , shouldLog'
    , isProductionOrStaging
    , isProductionServer
    , sessionDuration
    , updateSessionMap -- exported for testing only
    , keyToInt
    ) where

import           Blaze.ByteString.Builder (toByteString)
import           ClassyPrelude.Yesod hiding (pi)
import           Control.Concurrent.Lifted (myThreadId)
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Control.Monad.Trans.Resource (allocate, release)
import           Data.Aeson (withText)
import           Data.Conduit.Pool
import qualified Data.List
import           Data.Text (strip)
import           Database.Persist.Sql (runSqlConn,
                                       PersistentSqlException
                                           (Couldn'tGetSQLConnection))
import qualified Database.Persist.Sql as SQL
import           FP.EnvSettings (learningSiteAnalytics, learningSiteApproot,
                                 isProductionOrStaging, isProductionServer, fpEnvironmentType)
import           FP.Logger (syslogMessageLogger)
import qualified FP.Store.Blob as Blob
import           Import.DBTimed
import qualified Model
import           Model hiding (UniqueUser, UniqueIdent)
import           Network.Mail.Mime hiding (htmlPart)
import           Network.Wai as WAI
import qualified Settings
import           Settings (widgetFile)
import           Settings.Development (development)
import           Settings.StaticFiles
import           System.Posix.Types (CPid)
import           System.Random (randomIO, randomRIO)
import           System.Random.MWC (Gen)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Email.Validate
import           Text.Hamlet (hamletFile)
import           Yesod.Auth
import           Yesod.Auth.BrowserId
import           Yesod.Auth.Email
import qualified Yesod.Auth.GoogleEmail2 as G2
import qualified Yesod.Auth.Message
import           Yesod.Core.Types (Logger)
import           Yesod.RssFeed (rssLink)

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { getStatic :: Static -- ^ Settings for static file serving.
    , getWPStatic :: Static
    , connPool :: Pool (CPid, SqlBackend) -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    , appBlobStore :: Blob.Store
    , githubClientId :: ByteString
    , githubClientSecret :: ByteString
    , appRenderSendMail :: Manager -> Text -> (Address -> Mail) -> ResourceT IO ()
    , staticRoot :: Text -- ^ root for static URLs
    , appLogger :: Logger
    , appPrivate :: Bool
    , appRandom :: Gen (PrimState IO)
    , appGhcEnvs :: [GhcEnvId]
    , appClientSessionBackend :: SessionBackend
      -- ^ The client session backend used to store the server session key
    , googleOAuth :: (Text, Text)
    -- ^ client ID, client secret
    , appAdmins :: HashSet Text
    -- ^ Set of administrator e-mail addresses
    }

instance HasHttpManager App where
    getHttpManager = httpManager

instance FromJSON Day where
    parseJSON = withText "Day" $ \t ->
        case parseTime defaultTimeLocale "%F" (unpack t) of
          Just d -> pure d
          _      -> fail "could not parse ISO date"
    {-# INLINE parseJSON #-}

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm Handler (FormResult x, Widget)

serverSessionId :: Text
serverSessionId = "SERVER_SESSION"

getClearServerSessionR :: Handler ()
getClearServerSessionR = deleteSession serverSessionId

-- | Ensure that a server session is created for this user and added to their
-- client-side session.
setServerSessionIfMissing :: Maybe UserId -> YesodDB App ServerSessionId
setServerSessionIfMissing muid = do
    msid <- maybeServerSession muid
    maybe (setServerSession muid) return msid

-- | Create a new server session and set its key in the client side session.
setServerSession :: Maybe UserId -> YesodDB App ServerSessionId
setServerSession muid = do
    now <- liftIO getCurrentTime
    sid <- insert $ ServerSession muid now
    setSession serverSessionId $ toPathPiece sid
    return sid

maybeServerSession :: Maybe UserId -> YesodDB App (Maybe ServerSessionId)
maybeServerSession muid = do
    msid <- (>>= fromPathPiece) <$> lookupSession serverSessionId
    case msid of
        Nothing -> return Nothing
        Just sid -> do
            ms <- get sid
            case ms of
                Just (ServerSession muid' _)
                    | muid == muid' -> do
                        now <- liftIO getCurrentTime
                        update sid [ServerSessionWhen =. now]
                        return $ Just sid
                _ -> return Nothing

-- | Require that a server session be available. If not present, we create a
-- new one and set it in the user's client side session.
--
-- Note that, for logged in users, we tie the session down to the user ID to
-- inhibit hijacking.
requireServerSession :: Maybe UserId -> YesodDB App ServerSessionId -- FIXME cache
requireServerSession muid = do
    msid <- maybeServerSession muid
    case msid of
        Nothing -> setServerSession muid
        Just sid -> return sid

-- | Put something into the SessionLog table.
sessionLogDB :: Text -> YesodDB App ()
sessionLogDB msg = do
    tid <- myThreadId
    muid <- lift maybeAuthId
    msid <- maybeServerSession muid
    case msid of
        Nothing -> return ()
        Just sid -> do
            now <- liftIO getCurrentTime
            insert_ $ SessionLog sid now $ concat
                [ "["
                , tshow tid
                , "] "
                , msg
                ]

-- | Put something into the SessionLog table.
sessionLog :: Text -> Handler ()
sessionLog = $runDBT . sessionLogDB

requireProfile :: Handler (Entity Profile)
requireProfile = fmap unCurrentUserProfile $ cached $ fmap CurrentUserProfile $ do
    uid <- requireAuthId
    $runDBT $ do
        mprofile <- getBy $ UniqueProfile uid
        case mprofile of
            Nothing -> do
                (uhandle, nhandle) <- getNewHandle
                let profile = Profile
                        { profileUser = uid
                        , profileHandle = uhandle
                        , profileNormalizedHandle = nhandle
                        , profileDisplay = ""
                        , profileKeymap = Nothing
                        , profileGithubAccessKey = Nothing
                        , profileSshKeyPair = Nothing
                        , profileBio = Nothing
                        , profileCompany = ""
                        , profileHomepage = Nothing
                        , profileTelephone = Nothing
                        , profileTheme = Just "Panda"
                        , profileFontSize = 14
                        , profileSearchWithRegex = Just False
                        , profileSettingsSubstitutions = Nothing
                        , profileAutomatic = True
                        , profileDisqus = Nothing
                        , profileProjectTemplatesHandle = Nothing
                        }
                pid <- insert profile
                return $ Entity pid profile
            Just profile -> return profile
  where
    getNewHandle = do
        number <- liftIO $ randomRIO (1, 10000000 :: Int)
        let uhandle = UserHandle $ "fpuser" ++ tshow number
        case normalizeHandle uhandle of
            Left _ -> getNewHandle
            Right nhandle -> do
                mp <- getBy $ UniqueNormalizedHandle nhandle
                case mp of
                    Nothing -> return (uhandle, nhandle)
                    Just _ -> getNewHandle

newtype CurrentUserProfile = CurrentUserProfile { unCurrentUserProfile :: Entity Profile }
    deriving Typeable

defaultLayoutExtra :: Maybe Widget -- ^ top content
                   -> Maybe Widget -- ^ right-column content
                   -> Maybe Widget -- ^ extra footer content
                   -> [((Route App, [(Text, Text)]), Text)] -- ^ breadcrumbs
                   -> Maybe (Text -> Html)
                   -> Widget -- ^ page content
                   -> Handler Html
defaultLayoutExtra mtop maside mfooter (Data.List.zip [1..] -> breadcrumbs') mga widget = do
    mmsg <- getMessage
    ma <- maybeAuth

    _ <- $runDBT $ setServerSessionIfMissing $ entityKey <$> ma

    mp <-
        case ma of
            Nothing -> return Nothing
            Just _ -> Just <$> requireProfile

    needOldPassword' <-
        case ma of
            Nothing -> return True
            Just (Entity aid _) -> needOldPassword aid

    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.

    pc <- widgetToPageContent $ do
        defaultWidget

        rssLink RecentContentFeedR "Recently published content"

        monclick <-
            case ma of
                Just _ -> return Nothing
                Nothing -> Just <$> Yesod.Auth.BrowserId.createOnClick browserIdSettings AuthR

        let navbar = $(widgetFile "navbar")

        $(widgetFile "default-layout")

        -- If there's no side content, let the main content take up the whole width.
        case maside of
            Nothing -> toWidget [lucius| .main { width: auto; float: none; } |]
            Just _ -> return ()
    googleAnalytics <- maybe (makeGoogleAnalytics Nothing) return mga
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

defaultWidget :: Widget
defaultWidget = defaultWidgetJs >> defaultWidgetCss

defaultWidgetJs :: Widget
defaultWidgetJs = do
    addScriptEither urlJqueryJs
    $(combineScripts 'StaticR
        [ design_js_init_js
        , js_blockUI_js
        , bootstrap_js_bootstrap_js

        , codemirror_lib_codemirror_js
        , codemirror_addon_mode_multiplex_js
        , codemirror_addon_runmode_runmode_js

        , codemirror_mode_xml_xml_js
        , codemirror_mode_css_css_js
        , codemirror_mode_javascript_javascript_js
        , codemirror_mode_htmlmixed_htmlmixed_js
        , codemirror_mode_haskell_haskell_js
        , codemirror_mode_haskell_routes_js
        , codemirror_mode_haskell_shakespeare_shakespeare_js
        , codemirror_mode_haskell_shakespeare_css_js
        , codemirror_mode_haskell_shakespeare_julius_js
        , codemirror_mode_haskell_shakespeare_lucius_js
        , codemirror_mode_haskell_shakespeare_cassius_js
        , codemirror_mode_haskell_shakespeare_hamlet_js
        , codemirror_mode_haskell_shakespeare_yesod_js
        , codemirror_mode_clike_clike_js
        , codemirror_mode_markdown_markdown_js
        , codemirror_mode_yaml_yaml_js

        , codemirror_keymap_emacs_js
        , codemirror_keymap_vim_js

        , codemirror_addon_dialog_dialog_js
        , codemirror_addon_edit_closebrackets_js
        , codemirror_addon_edit_matchbrackets_js
        , codemirror_addon_edit_trailingspace_js
        , codemirror_addon_hint_show_hint_js
        , codemirror_addon_search_search_js
        , codemirror_addon_search_searchcursor_js
        , codemirror_addon_search_match_highlighter_js
        , codemirror_addon_selection_active_line_js

        , js_fpco_codemirror_js
        ])
    codeMirror
  where
    codeMirror :: Widget
    codeMirror = do
        keyMap <- liftHandlerT getKeyMap

        $(widgetFile "codemirror")

    getKeyMap = do
        muid <- maybeAuthId
        mprofile <-
            case muid of
                Nothing -> return Nothing
                Just uid -> $runDBT $ getBy $ UniqueProfile uid
        return $
            case mprofile >>= profileKeymap . entityVal of
                Nothing -> asText ""
                Just KeymapVim -> "vim"
                Just KeymapEmacs -> "emacs"

defaultWidgetCss :: Widget
defaultWidgetCss = do
    $(combineStylesheets 'StaticR
        [
          codemirror_lib_codemirror_css

        , codemirror_addon_dialog_dialog_css
        , codemirror_addon_hint_show_hint_css

        , bootstrap_css_bootstrap_css
        , font_awesome_css_font_awesome_css
        ])

-- | Find deltas created between original and new map. If there are any deltas,
-- return a function to apply those deltas to another map.
updateSessionMap :: SessionMap -- ^ original map
                 -> SessionMap -- ^ new map
                 -> Maybe (SessionMap -> SessionMap) -- ^ Nothing if no changes
updateSessionMap orig0 new0 =
    loop (mapToList orig0) (mapToList new0) False id
  where
    loop :: [(Text, ByteString)] -- ^ original
         -> [(Text, ByteString)] -- ^ new
         -> Bool -- ^ have we noticed any changes so far?
         -> (SessionMap -> SessionMap) -- ^ built up modification function
         -> Maybe (SessionMap -> SessionMap)
    loop [] [] False _ = Nothing
    loop [] [] True  f = Just f
    loop [] ((k2, v2):new) _ f = loop [] new True (f . insertMap k2 v2)
    loop ((k1, _):orig) [] _ f = loop orig [] True (f . deleteMap k1)
    loop ((k1, v1):orig) ((k2, v2):new) changed f =
        case compare k1 k2 of
            LT -> loop orig ((k2, v2):new) True (f . deleteMap k1)
            GT -> loop ((k1, v1):orig) new True (f . insertMap k2 v2)
            EQ
                | v1 == v2  -> loop orig new changed f
                | otherwise -> loop orig new True (f . insertMap k2 v2)

shouldLog' :: Text -> LogLevel -> Bool
shouldLog' source level =
    (development && source /= "SQL")
        || level == LevelWarn
        || level == LevelError
        || source == "PRUNING"
        || source == "TIMING"
        || source == "FAY-CALL"

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootStatic learningSiteApproot

    defaultLayout = defaultLayoutExtra Nothing Nothing Nothing [] Nothing

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (staticRoot y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    shouldLog _ = shouldLog'

    isAuthorized route _
        | "admin" `member` routeAttrs route = do
            app <- getYesod
            muser <- maybeAuth
            case muser of
                Nothing -> return AuthenticationRequired
                Just (Entity _ user) -> do
                    x <- isAdmin app user
                    return $ if x
                        then Authorized
                        else Unauthorized "Admin access only"
        | otherwise = return Authorized

    maximumContentLength _ (Just MediaUploadR{}) = Just 300000
    maximumContentLength _ _ = Just (2 * 1024 * 1024) -- 2MB

    -- Avoid path rewriting for the Wordpress business site
    cleanPath _ s@("business":_) = Right s
    cleanPath _ s@("wp-content":_) = Right s
    cleanPath _ s@("wp-includes":_) = Right s
    cleanPath _ s = -- FIXME in yesod-core, export defaultCleanPath
        if corrected == s
            then Right $ map dropDash s
            else Left corrected
      where
        corrected = filter (not . null) s
        dropDash t
            | all (== '-') t = drop 1 t
            | otherwise = t

    messageLoggerSource a _ = syslogMessageLogger (shouldLogIO a)

    errorHandler e =
        case e of
            NotFound -> selectRep $ do
              provideRep $ getNotFoundPage return
              provideRep $ return $ object ["message" .= ("Not Found" :: Text)]
            _ -> defaultErrorHandler e

getNotFoundPage :: (Widget -> Handler Widget) -> Handler Html
getNotFoundPage f = do
    widget <- f $ do
        setTitle "404 Not Found"
        $(widgetFile "not-found")
    defaultLayoutExtra Nothing (Just mempty) Nothing [] Nothing widget

isAdmin :: MonadIO m => App -> User -> m Bool
isAdmin app user =
    liftIO $ fmap (isEmailAdmin (userEmail user)) $
        return (appAdmins app)

isEmailAdmin :: Text -> HashSet Text -> Bool
isEmailAdmin email admins = (email `member` admins) || nonProdFP
  where
    nonProdFP = not isProductionOrStaging && fpEnvironmentType /= "development" &&
                "@fpcomplete.com" `isSuffixOf` email

browserIdSettings :: BrowserIdSettings
browserIdSettings = def

-- How to run database actions.
instance PidConnectionPool App where
    pidConnectionPool = connPool
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB f = do
        master <- getYesod
        mres <- withResourceTimeout 2000000 (connPool master)
              $ runSqlConn f . snd
        maybe (throwIO Couldn'tGetSQLConnection) return mres
instance YesodPersistRunner App where
    getDBRunner = do
        ididSucceed <- newIORef False

        pool <- fmap connPool getYesod
        managedConn <- takeResource pool
        let conn = snd $ mrValue managedConn

        let withPrep f = f conn (SQL.connPrepare conn)
        (finishTransaction, ()) <- allocate (withPrep SQL.connBegin) $ \() -> do
            didSucceed <- readIORef ididSucceed
            withPrep $ if didSucceed
                then SQL.connCommit
                else SQL.connRollback

        let cleanup = do
                writeIORef ididSucceed True
                release finishTransaction
                mrReuse managedConn True
                mrRelease managedConn

        return (DBRunner $ \x -> runReaderT x conn, cleanup)

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = DashboardR
    -- Where to send a user after logout
    logoutDest _ = HomeR

    redirectToReferer _ = True

    -- Suppress "You are now logged in" message
    onLogin = return ()

    onLogout = return ()

    getAuthId creds = join $ $runDBT $ do
        let email = credsIdent creds
        x <- getUserByEmail False email
        now <- liftIO getCurrentTime
        Entity uid user <-
            case x of
                Just uent -> return uent
                Nothing -> do
                    muser <- getBy $ Model.UniqueUser email
                    uent@(Entity uid _) <-
                        case muser of
                            Nothing -> do
                                st <- newSecurityToken
                                let user = User
                                        { userEmail = email
                                        , userPassword = Nothing
                                        , userVerkey = Nothing
                                        , userVerified = True
                                        , userSecurityToken = st
                                        , userNeedsNewPassword = credsPlugin creds == "email"
                                        }
                                uid <- insert user
                                return $ Entity uid user
                            Just uent -> return uent
                    insert_ $ Ident uid email (mkLowerCaseText email) True

                    return uent
        insert_ $ Login (keyToInt uid) (Just (userEmail user)) now $ credsPlugin creds
        return $ return $ Just uid

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins app =
        [ authBrowserId browserIdSettings
        , authEmail
        , uncurry G2.authGoogleEmail (googleOAuth app)
        ]

    authHttpManager = httpManager

    loginHandler = lift $ do
        -- If we're already logged in, redirect appropriately
        ma <- maybeAuth
        case ma of
            Just {} -> getYesod >>= redirectUltDest . loginDest
            Nothing -> defaultLayoutExtra Nothing (Just mempty) Nothing [] Nothing $ do
                setTitle "Log in to School of Haskell"
                $(widgetFile "login")

    renderAuthMessage _ _ Yesod.Auth.Message.Register = "Send confirmation link"
    renderAuthMessage _ _ msg = Yesod.Auth.Message.defaultMessage msg

    -- Override the default to allow security token authentication and enforce
    -- two-factor authentication.
    maybeAuthId = do
        req <- waiRequest
        muid <-
            case words . decodeUtf8 <$> lookup "authorization" (WAI.requestHeaders req) of
                Just ["token", t] -> fmap (fmap entityKey) $ $runDBT $ getBy $ UniqueSecurityToken $ SecurityToken t
                _ -> return Nothing
        case muid of
            Nothing -> checkSessionKey
            Just uid -> return $ Just uid
      where
        checkSessionKey = do
            ms <- lookupSession credsSessionKey
            case ms of
                Nothing -> return Nothing
                Just s ->
                    case fromPathPiece s of
                        Nothing -> return Nothing
                        Just aid -> fmap (fmap entityKey) $ cachedAuth aid

        cachedAuth aid = runMaybeT $ do
            a <- MaybeT $ fmap unCachedMaybeAuth
                        $ cached
                        $ fmap CachedMaybeAuth
                        $ $runDBT
                        $ get aid
            return $ Entity aid a

        credsSessionKey = "_ID"

instance YesodAuthPersist App

newtype CachedMaybeAuth = CachedMaybeAuth { unCachedMaybeAuth :: Maybe User }
    deriving (Typeable)

getServerSessionKey :: Handler (Maybe ByteString)
getServerSessionKey = do
    app <- getYesod
    let SessionBackend client = appClientSessionBackend app
    req <- waiRequest
    (clientMap, _) <- liftIO $ client req
    return $ lookup serverSessionKey clientMap

serverSessionKey :: Text
serverSessionKey = "_SERVER_SESSION"

sessionDuration :: Int
sessionDuration = (60 * 60 * 24 * 7) -- one week

getUserByEmail :: Bool -> Text -> YesodDB App (Maybe (Entity User))
getUserByEmail onlyPrimary =
    maybe (return Nothing) go . canon
  where
    canon = fmap decodeUtf8 . Text.Email.Validate.canonicalizeEmail . encodeUtf8
    go email = do
        idents <- selectList [IdentLident ==. mkLowerCaseText email, IdentEmail ==. True] [Asc IdentId]
        case idents of
            [] -> getBy $ Model.UniqueUser email
            [Entity _ i] -> returnIdent i
            Entity _ firstMatch:_ -> do
                mi <- getBy $ Model.UniqueIdent email
                returnIdent $ maybe firstMatch entityVal mi
      where
        returnIdent i = do
            let uid = identUser i
            u <- get404 uid
            if onlyPrimary && mkLowerCaseText (userEmail u) /= mkLowerCaseText email
                then return Nothing
                else return $ Just $ Entity uid u

getUserByEmail404 :: Bool -> Text -> YesodDB App (Entity User)
getUserByEmail404 x y = getUserByEmail x y >>= maybe notFound return

sendVerifyEmailHelper :: Text -> Text -> Handler ()
sendVerifyEmailHelper email verkey = do
    Entity uid _ <- $runDBT $ getUserByEmail404 True email
    y <- getYesod
    render <- getUrlRenderParams
    let verurl = VerifyEmailR uid verkey
        html = $(hamletFile "templates/verify-email.hamlet") render
    liftResourceT $ appRenderSendMail y (httpManager y) email $ \from -> (emptyMail from)
        { mailTo = [Address Nothing email]
        , mailHeaders =
            [ ("Subject", "Welcome to School of Haskell")
            ]
        , mailParts = [[htmlPart html]]
        }
  where
    htmlPart html = Part
        { partType = "text/html; charset=utf-8"
        , partEncoding = None
        , partFilename = Nothing
        , partContent = renderHtml html
        , partHeaders = []
        }

addUnverifiedDB :: Text -> Text -> YesodDB App UserId
addUnverifiedDB email verkey = do
    st <- newSecurityToken
    insert User
        { userEmail = email
        , userPassword = Nothing
        , userVerkey = Just verkey
        , userVerified = False
        , userSecurityToken = st
        , userNeedsNewPassword = True
        }

instance YesodAuthEmail App where
    type AuthEmailId App = UserId

    afterPasswordRoute _ = ProfileR

    addUnverified email verkey = $runDBT $ addUnverifiedDB email verkey

    sendVerifyEmail email verkey _ = sendVerifyEmailHelper email verkey
    getVerifyKey = $runDBT . fmap (join . fmap userVerkey) . get
    setVerifyKey uid key = $runDBT $ update uid [UserVerkey =. Just key]
    verifyAccount uid = $runDBT $ do
        mu <- get uid
        case mu of
            Nothing -> return Nothing
            Just _ -> do
                update uid [UserVerified =. True]
                return $ Just uid
    getPassword = $runDBT . fmap (join . fmap userPassword) . get
    setPassword uid pass = $runDBT $ update uid [UserPassword =. Just pass, UserNeedsNewPassword =. False]
    getEmailCreds email = $runDBT $ do
        mu' <- getUserByEmail False email
        mu <-
            case mu' of
                Nothing -> do
                    mp <-
                        case normalizeHandle $ UserHandle $ strip email of
                            Left _ -> return Nothing
                            Right nh -> getBy $ UniqueNormalizedHandle nh
                    case mp of
                        Nothing -> return Nothing
                        Just (Entity _ p) -> do
                            let uid = profileUser p
                            u <- get404 uid
                            return $ Just $ Entity uid u
                Just x -> return $ Just x
        case mu of
            Nothing -> return Nothing
            Just (Entity uid u) -> return $ Just EmailCreds
                { emailCredsId = uid
                , emailCredsAuthId = Just uid
                , emailCredsStatus = isJust $ userPassword u
                , emailCredsVerkey = userVerkey u
                , emailCredsEmail = userEmail u
                }
    getEmail = $runDBT . fmap (fmap userEmail) . get

    checkPasswordSecurity _ t = return $ checkPasswordSecurity' t

    confirmationEmailSentResponse identifier = fmap toTypedContent $ defaultLayout $ do
        setTitle "Confirmation email sent"
        $(widgetFile "confirmation-email-sent")

    normalizeEmailAddress _ = toLower

checkPasswordSecurity' :: Text -> Either Text ()
checkPasswordSecurity' t
    | length t < 3 = Left "Password must be at least three characters"
    | otherwise = Right ()

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

urlJqueryJs :: Either (Route App) Text
urlJqueryJs = if development then Left (StaticR js_jquery_js) else Right "//ajax.googleapis.com/ajax/libs/jquery/1.8.2/jquery.min.js"

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email

prettyProfile :: Profile -> Text
prettyProfile p
    | null (profileDisplay p) = unUserHandle (profileHandle p)
    | otherwise = profileDisplay p

newSecurityToken :: (MonadResource m, MonadLogger m) => SqlPersistT m SecurityToken
newSecurityToken = do
    s <- liftIO randomIO
    mu <- getBy $ UniqueSecurityToken s
    case mu of
        Nothing -> return s
        Just _ -> newSecurityToken

-- | Make a Google Analytics generator from the current session.
makeGoogleAnalytics
  :: (MonadHandler m,HandlerSite m ~ App,m ~ HandlerT App IO)
  => Maybe Text -- ^ An optional path to send to the tracker instead of
                -- the current URL's path.
  -> m (Text -> Html)
makeGoogleAnalytics mpath = do
  loggedIn <- fmap isJust maybeAuthId
  let userTypeValue :: Text
      userTypeValue | loggedIn = "Member"
                    | otherwise = "Visitor"
      userTypeScope :: Text
      userTypeScope | loggedIn = "2"  -- Session-level.
                    | otherwise = "1" -- Visitor-level.
  return $ \section ->
    [shamlet|
        $maybe analytics <- learningSiteAnalytics
            <script>
              if(!window.location.href.match(/localhost/)){
                var track = '#{path}';
                window._gaq = [['_setAccount','#{analytics}']
                              ,track != ''? ['_trackPageview',track] : ['_trackPageview']
                              ,['_trackPageLoadTime']];
                window._gaq.push(['_setCustomVar',1,'Section','#{section}',3]);
                window._gaq.push(['_setCustomVar',2,'User Type','#{userTypeValue}',#{userTypeScope}]);
                (function() {
                \  var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
                \  ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
                \  var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
                \
                })();
              }
    |]

  where path :: Text
        path = fromMaybe "" mpath

-- | Make a path for Google Analytics based on the current route.
makeGAPath :: (Text -> Text) -> Handler (Maybe Text)
makeGAPath f = do
  app <- getYesod
  currRoute <- getCurrentRoute
  return $ do
    (parts,queries) <- fmap renderRoute currRoute
    return (f (decodeUtf8 (toByteString (joinPath app "" parts queries))))

-- | Like a standard requireAuth, but if user is redirected to login page, sets
-- a message first.
requireAuthMsg :: Bool -- ^ set ult destination to the current page?
               -> Html
               -> Handler (Entity User)
requireAuthMsg ud msg = do
    ma <- maybeAuth
    case ma of
        Nothing -> do
            setMessage msg
            when ud $ getCurrentRoute >>= mapM_ setUltDest
            redirect $ AuthR LoginR
        Just a -> return a

randomString :: Handler Text
randomString = getYesod >>= liftIO . randomKey

keyToInt :: ( ToBackendKey (PersistEntityBackend r) r
            , Enum (BackendKey (PersistEntityBackend r))
            ) => Key r -> Int
keyToInt = fromEnum . toBackendKey
