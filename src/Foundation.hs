{-# OPTIONS -fno-warn-orphans -O1 #-}
module Foundation
    ( App (..)
    , ExportStatus (..)
    , Route (..)
    , resourcesApp
    , Handler
    , Widget
    , Form
    , module Settings
    , module Model
    , defaultLayoutExtra
    , prettyProfile
    , defaultWidget
    , defaultWidgetJs
    , getNotFoundPage
    , isAdmin
    , makeGoogleAnalytics
    , makeGAPath
    , Import.DBTimed.runDB
    , getUserByEmail
    , getUserByEmail404
    , getClearServerSessionR
    , shouldLog'
    , isProductionOrStaging
    , isProductionServer
    , sessionDuration
    , updateSessionMap -- exported for testing only
    , keyToInt
    , profileByHandle
    ) where

import           Blaze.ByteString.Builder (toByteString)
import           ClassyPrelude.Yesod hiding (pi)
import           Control.Monad.Trans.Resource (allocate, release)
import           Data.Aeson (withText)
import           Data.Conduit.Pool
import qualified Data.List
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
import qualified Settings
import           Settings (widgetFile)
import           Settings.Development (development)
import           Settings.StaticFiles
import           System.Posix.Types (CPid)
import           System.Random.MWC (Gen)
import qualified Text.Email.Validate
import           Text.Hamlet (hamletFile)
import           Yesod.Core.Types (Logger)
import           Yesod.RssFeed (rssLink)

data ExportStatus
  = ExportSuccess !String -- ^ file
  | ExportError !String
  | ExportNotComplete

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
    , appExportStatus :: IORef ExportStatus
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

defaultLayoutExtra :: Maybe Widget -- ^ top content
                   -> Maybe Widget -- ^ right-column content
                   -> Maybe Widget -- ^ extra footer content
                   -> [((Route App, [(Text, Text)]), Text)] -- ^ breadcrumbs
                   -> Maybe (Text -> Html)
                   -> Widget -- ^ page content
                   -> Handler Html
defaultLayoutExtra mtop maside mfooter (Data.List.zip [1..] -> breadcrumbs') mga widget = do
    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.

    pc <- widgetToPageContent $ do
        defaultWidget

        rssLink RecentContentFeedR "Recently published content"

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
        ])

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

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    shouldLog _ = shouldLog'

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

-- | Make a Google Analytics generator from the current session.
makeGoogleAnalytics
  :: (MonadHandler m,HandlerSite m ~ App,m ~ HandlerT App IO)
  => Maybe Text -- ^ An optional path to send to the tracker instead of
                -- the current URL's path.
  -> m (Text -> Html)
makeGoogleAnalytics mpath = do
  let userTypeValue :: Text
      userTypeValue = "Visitor"
      userTypeScope :: Text
      userTypeScope = "1" -- Visitor-level.
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

keyToInt :: ( ToBackendKey (PersistEntityBackend r) r
            , Enum (BackendKey (PersistEntityBackend r))
            ) => Key r -> Int
keyToInt = fromEnum . toBackendKey

profileByHandle :: UserHandle -> YesodDB App (Entity Profile)

-- MS 2013-04-21 The following should be sufficient, but I got segfaults when
-- using it. The segfaults seem to be a bug in GHC, and one I've encountered
-- before. I'm not sure if it's simply a matter of a badly compiled library,
-- but I want to make sure the bug never rears its head in production.
-- Therefore, writing this the long way instead.
--
--profileByHandle = either (const $ lift notFound) (getBy404 . UniqueNormalizedHandle) . normalizeHandle

profileByHandle uh =
    case normalizeHandle uh of
        Left _ -> lift notFound
        Right nh -> do
            mp <- getBy $ UniqueNormalizedHandle nh
            case mp of
                Nothing -> notFound
                Just p -> return p
