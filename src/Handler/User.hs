module Handler.User where

import Import hiding (breadcrumbs)

getUserR :: UserHandle -> Handler Html
getUserR (UserHandle "school") = redirect HomeR
getUserR uh = do
  ga <- makeGoogleAnalytics Nothing
  groupHelper uh [] Nothing Nothing Nothing Nothing ga

getHomeR :: Handler Html
getHomeR = do
  ga <- makeGoogleAnalytics Nothing
  groupHelper (UserHandle "school") [] Nothing Nothing Nothing Nothing ga

getOldSchoolR :: Handler Html
getOldSchoolR = redirectWith movedPermanently301 HomeR

-- WARNING: The code below is hideously ugly with lots of weird conditionals.
-- It was cobbled together by merging three similar pieces of code (SoH
-- homepage, UserR, and group display). The code needs to be cleaned up, but
-- seems to be doing the right thing currently.
handleTCGroup :: Profile
              -> [((Route App, [(Text, Text)]), Text)]
              -> TutorialName
              -> [TutorialName]
              -> UserId
              -> Text
              -> Textarea
              -> [HolderContent (Maybe UTCTime) (Route App, [(Text, Text)])]
              -> (Text -> Html)
              -> Handler Html
handleTCGroup profile bcs tn tns authorid pageTitle description contents ga = do
    author <- $runDB $ get404 authorid
    groupHelper
        (profileHandle profile)
        (tn:tns)
        (Just (author, authorid, profile, contents))
        (Just bcs)
        (Just description)
        (Just pageTitle)
        ga

groupHelper :: UserHandle
            -> [TutorialName]
            -> Maybe (User, UserId, Profile, [HolderContent (Maybe UTCTime) (Route App, [(Text, Text)])])
            -> Maybe [((Route App, [(Text, Text)]), Text)]
            -> Maybe Textarea
            -> Maybe Text
            -> (Text -> Html)
            -> Handler Html
groupHelper uh allNames mcontents mbcs mdescription mpageTitle ga = do
    let meditGroup =
            case allNames of
                [] -> Nothing
                tn:tns -> Just $ EditGroupR tn tns
    ma <- maybeAuth

    admin <-
        case ma of
            Nothing -> return False
            Just (Entity _ user) -> do
                app <- getYesod
                isAdmin app user

    (user, uid, profile, contents) <-
        case mcontents of
            Nothing -> $runDB $ do
                Entity _ profile <- profileByHandle uh
                let uid = profileUser profile
                user <- get404 $ profileUser profile
                contents <- getTopContents GetTutPath
                    { gtpShowPrivates = Just uid == fmap entityKey ma
                    , gtpShowTutorials = True
                    }
                    (\x -> (UserTutorialR uh x [], []))
                    (\x ->
                        let (tn:tns) = allNames ++ [x]
                         in (DeleteMemberR tn tns, []))
                    uid
                return (user, uid, profile, filter (not . hiddenGroup uid) contents)

              where hiddenGroup uid g = uh == UserHandle "school" &&
                                        not (Just uid == fmap entityKey ma) &&
                                        hcSlug g == Just (TutorialName "project-templates")

            Just x -> return x
    let mprofile
            | schoolHome = Nothing
            | null allNames = Just profile
            | otherwise = Nothing
        pageTitle =
            case mpageTitle of
                Nothing
                    | schoolHome -> "School of Haskell"
                    | otherwise -> prettyProfile profile
                Just pt -> pt
        isCurrentUser = Just uid == fmap entityKey ma
        schoolHome = uh == UserHandle "school" && not isCurrentUser
    let header
            | schoolHome && null allNames = Nothing
            | otherwise = Just $(widgetFile "user-header")
        breadcrumbs =
            case mbcs of
                Just bcs -> bcs
                Nothing
                    | schoolHome -> []
                    | otherwise -> [((UserR uh, []), prettyProfile profile)]
        displayGravatars = schoolHome
    (tutHeader, tutFooter, tutSidebar) <- getTutorialExtras
    msidebar <-
        case mdescription of
            Nothing -> sidebarHelper tutSidebar
                     $ fmap (TutorialContent' . unTextarea)
                     $ profileBio profile
            Just description -> sidebarHelper tutSidebar
                              $ Just $ TutorialContent' $ unTextarea description
    defaultLayoutExtra header (Just msidebar) Nothing breadcrumbs (Just ga) $ do
        setTitle $ toHtml $ pageTitle ++ if null allNames then "" else " - School of Haskell"
        when (schoolHome && null allNames) [whamlet|<h1 itemprop=name>School of Haskell|]
        when admin
            [whamlet|
                <p>
                    Admin only: the user's email address is
                    <a href=mailto:#{userEmail user}>#{userEmail user}
                <p>
                    <a href=@{AdminR}>Admin page
            |]
        let maybeNoContent
                | schoolHome || not (null allNames) = Nothing
                | isCurrentUser = Just $(widgetFile "blank-my-content")
                | otherwise = Just [whamlet|<p>This user has not published any content.|]
            maside = Just msidebar
        $(widgetFile "group-contents")

sidebarHelper :: Html -- ^ tutorial sidebar
              -> Maybe TutorialContent -> Handler Widget
sidebarHelper tutSidebar msidebarContent =
    withPadding . snd <$> sidebarContent
  where
    sidebarContent = maybe (return mempty) displayTutorial msidebarContent
    --TODO: tidier
    withPadding body =
      [whamlet|
        $newline never
        <div style=padding-top:20px; itemprop=desc>
            ^{body}
            \#{tutSidebar}
      |]
