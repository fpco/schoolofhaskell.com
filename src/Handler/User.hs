module Handler.User
  ( handleTCGroup
  , getOldSchoolR
  , getHomeR
  , getUserR
  ) where

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
    (user, _uid, profile, contents) <-
        case mcontents of
            Nothing -> $runDB $ do
                Entity _ profile <- profileByHandle uh
                let uid = profileUser profile
                user <- get404 $ profileUser profile
                contents <- getTopContents
                    (\x -> (UserTutorialR uh x [], []))
                    uid
                return (user, uid, profile, filter (not . hiddenGroup) contents)

              where hiddenGroup g = uh == UserHandle "school" &&
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
        schoolHome = uh == UserHandle "school"
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
        let maybeNoContent
                | schoolHome || not (null allNames) = Nothing
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
