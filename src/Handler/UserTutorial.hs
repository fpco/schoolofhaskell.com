module Handler.UserTutorial where

import Data.Text (splitOn)
import Handler.User
import Import hiding (DList)

getUserTutorialR :: UserHandle -> TutorialName -> [TutorialName] -> Handler Html
getUserTutorialR (UserHandle uh) tn tns | toLower uh == "school" = redirect $ SchoolTutorialR tn tns
getUserTutorialR uh tn tns = do
    Entity _ profile <- $runDB $ profileByHandle uh
    tutorialHelper (\params -> (((UserR uh, params), prettyProfile profile):)) profile (UserTutorialR uh) tn tns

getSchoolTutorialR :: TutorialName -> [TutorialName] -> Handler Html
getSchoolTutorialR tn tns = do
    Entity _ profile <- $runDB $ profileByHandle $ UserHandle "school"
    tutorialHelper (\params -> (((HomeR, params), "School of Haskell"):)) profile SchoolTutorialR tn tns

type DList a = [a] -> [a]
type RouteParams = (Route App, [(Text, Text)])

tutorialHelper :: ([(Text, Text)] -> DList (RouteParams, Text))
               -> Profile
               -> (TutorialName -> [TutorialName] -> Route App)
               -> TutorialName
               -> [TutorialName]
               -> Handler Html
tutorialHelper bcsFront profile toURL0 tn tns = do
    ma <- maybeAuth

    let toURL x y = (toURL0 x y, [])
        toDeleteURL x y = (DeleteMemberR x y, [])

    let isOwner = Just (profileUser profile) == fmap entityKey ma

    let location = intercalate "/" $
          unUserHandle (profileHandle profile) : map unTutorialName (tn : tns)

    (bcs, tc) <- $runDB $ do
        mres <- getTutPath GetTutPath
            { gtpShowPrivates = isOwner
            , gtpShowTutorials = True
            } (profileUser profile) toURL toDeleteURL tn tns
        case mres :: Maybe ([(RouteParams, Text)], TutContent RouteParams, TcontentId) of
            Nothing -> do
                -- If we can't find our content at the current location, check past locations.
                -- For more information, see:
                -- https://github.com/fpco/fpco/issues/702
                mcl <- getBy $ UniqueContentLocation location
                -- Huh... using getBy404 caused a segfault, the following seems
                -- to work just fine.
                case mcl of
                    Nothing -> lift (notFoundMaybeLogin ma)
                    Just (Entity _ (ContentLocation _ lcid)) -> do
                        mcanroute <- getCanonicalRouteContent lcid
                        lift $ maybe (notFoundMaybeLogin ma) (redirectWith status301) mcanroute
            Just (bcs :: [(RouteParams, Text)], tc :: TutContent RouteParams, cid :: TcontentId) -> do
                mlc <- getBy $ UniqueContentLocation location
                case mlc of
                    Just (Entity locid (ContentLocation _ lcid)) ->
                        unless (cid == lcid) $ replace locid $ ContentLocation location cid
                    Nothing -> insert_ $ ContentLocation location cid
                mcanroute <- getCanonicalRouteContent cid
                case mcanroute of
                    Nothing -> return ()
                    Just url -> lift $ do
                        currRoute <- getCurrentRoute
                        unless (currRoute == mcanroute) $ do
                            render <- getUrlRender
                            addHeader "Link" $ concat
                                [ "<"
                                , render url
                                , ">; rel=\"canonical\""
                                ]
                return (bcsFront [] bcs, tc)

    case tc of
        TCTutorial authorid Nothing mprev mnext (Entity _tid tutorial@Tutorial {..}) -> do
            (maside,body) <- displayTutorial tutorialContent
            (header, footer, sidebar) <- getTutorialExtras
            Entity _ author <- $runDB $ getBy404 $ UniqueProfile authorid
            let mgroup =
                    case drop 1 $ reverse bcs of
                        x:_parent:_rest -> Just x
                        _ -> Nothing
                mkLink newLast
                    | null tns = UserTutorialR (profileHandle profile) newLast []
                    | otherwise = UserTutorialR (profileHandle profile) tn $ replaceLast tns newLast
                related :: [(Route App, Text)]
                related = []
                bottomFooter = $(widgetFile "creative-commons")
            defaultLayoutExtra Nothing Nothing (Just bottomFooter) bcs Nothing $ do
                setTitle $ toHtml $ unTitle tutorialTitle ++ " - School of Haskell"
                let relatedContent = $(widgetFile "related-content")
                $(widgetFile "user-tutorial-unpublished")
        TCTutorial authorid (Just PublishedTutorial {..}) mprev mnext (Entity tid tutorial@Tutorial {..}) -> do
            -- Get related links
            -- FIXME: re-enable something like this

            -- let path = intercalate "/"
            --          $ unUserHandle (profileHandle profile)
            --          : map unTutorialName (tn : tns)
            -- related <- $runDB
            --          $ fmap catMaybes
            --          $ mapM getRelated
            --          $ maybe [] unpack
            --          $ lookup path relLinksMap
            let related :: [(Route App, Text)]
                related = []

            Entity _ author <- $runDB $ getBy404 $ UniqueProfile authorid
            (maside, body) <- displayTutorial publishedTutorialContent
            (header, footer, sidebar) <- getTutorialExtras

            let meditLink =
                     if isOwner
                         then Just $ EditTutorialR tn tns
                         else Nothing
            tcid <- getTutorialContentId tid

            let tags = [] :: [Text] -- FIXME implement tags
                header' = Just $(widgetFile "published-header")
            let mgroup =
                    case drop 1 $ reverse bcs of
                        x:_parent:_rest -> Just x
                        _ -> Nothing
                mkLink newLast
                    | null tns = UserTutorialR (profileHandle profile) newLast []
                    | otherwise = UserTutorialR (profileHandle profile) tn $ replaceLast tns newLast

            mpath <- makeGAPath ("/tutorial" <>)
            ga <- makeGoogleAnalytics mpath
            defaultLayoutExtra header' maside Nothing bcs (Just ga) $ do
                setTitle $ toHtml $ unTitle publishedTutorialTitle ++ " - School of Haskell"
                let relatedContent = $(widgetFile "related-content")
                $(widgetFile "user-tutorial")
                $(widgetFile "published-footer")
        TCGroup author pageTitle description contents -> do
          when (null contents && not isOwner) notFound
          mpath <- makeGAPath ("/group" <>)
          ga <- makeGoogleAnalytics mpath
          handleTCGroup profile bcs tn tns author (unTitle pageTitle)
                        description contents ga

notFoundMaybeLogin :: Maybe (Entity User) -> Handler a
notFoundMaybeLogin ma = do
    let notLoggedIn = isNothing ma
    content <- getNotFoundPage $ \page -> return
        [whamlet|
            ^{page}
            $if notLoggedIn
                <br>
                <p>Maybe you need to <a href=@{AuthR LoginR}>login</a>?
        |]
    sendResponseStatus status404 content

-- | Get the URL and title of the given tutorial path.
getRelated :: Text -- ^ path, e.g. username/group/tutorial
           -> YesodDB App (Maybe (Route App, Title))
getRelated path =
    case splitOn "/" path of
        (UserHandle -> user):(TutorialName -> tn):(map TutorialName -> tns) -> do
            mp <-
                case normalizeHandle user of
                    Left _ -> return Nothing
                    Right nh -> getBy $ UniqueNormalizedHandle nh
            case mp of
                Just (Entity _ Profile {..}) -> do
                    let toURL = UserTutorialR user
                    mres <- getTutPath GetTutPath
                        { gtpShowPrivates = False
                        , gtpShowTutorials = True
                        } profileUser (\_ _ -> ()) (\_ _ -> ()) tn tns
                    case mres of
                        Just (_, TCTutorial _ (Just PublishedTutorial {..}) _ _ _, _) -> do
                            return $ Just (toURL tn tns, publishedTutorialTitle)
                        _ -> return Nothing
                Nothing -> return Nothing
        _ -> return Nothing

replaceLast :: [a] -> a -> [a]
replaceLast [] _ = []
replaceLast [_] x = [x]
replaceLast (x:xs) y = x : replaceLast xs y

disqusWidget :: Profile -> Widget
disqusWidget profile = do
    mshortname <-
        case fromMaybe NoComments $ profileDisqus profile of
            NoComments -> return Nothing
            FPAccount -> return $ Just "fpcomplete" -- FIXME this probably should be hardcoded. Look in templates/blog-post.hamlet as well
            UserAccount t -> return $ Just $ unDisqusIdent t
    case mshortname of
        Nothing -> return ()
        Just shortname ->
            [whamlet|
                <div #disqus_thread>
                  <script>
                      var disqus_shortname = "#{shortname}"; (function() { var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true; dsq.src = 'https://' + disqus_shortname + '.disqus.com/embed.js'; (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq); })();
                  <noscript>Please enable JavaScript to view the <a href="https://disqus.com/?ref_noscript">comments powered by Disqus.</a>
                  <a href="https://disqus.com" class="dsq-brlink">comments powered by <span class="logo-disqus">Disqus</span></a>
            |]
