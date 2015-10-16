module Handler.EditTutorial where

import Import
import Handler.EditGroup (slugField)
import Text.Markdown (markdown)
import Data.Time (addUTCTime)

data Meta = Meta
    { metaTitle :: Title
    , metaDesc :: Textarea
    , metaSlug :: TutorialName
    , metaEnv  :: MaybeEnv
    }

data MaybeEnv = DefaultEnv | Env GhcEnvId
    deriving Eq

form :: Entity Tmember -> Tutorial -> [GhcEnvId] -> Form Meta
form memEnt@(Entity _ Tmember {..}) Tutorial {..} ghcEnvs = renderTable $ Meta
    <$> areq titleField "Title" (Just tutorialTitle)
    <*> (fromMaybe (Textarea "") <$> aopt textareaField "Description" (Just $ Just tutorialDesc))
    <*> areq (slugField memEnt) "Slug" { fsTooltip = Just "Used in URLs" } (Just tmemberSlug)
    <*> areq (selectFieldList envsList) "Package set" (Just $ maybe DefaultEnv Env tutorialEnvironment)
  where
    envsList = ("Default package set", DefaultEnv)
             : map go ghcEnvs
    go id' = (ghcEnvTitle id', Env id')

ghcEnvTitle :: GhcEnvId -> Text
ghcEnvTitle (GhcEnvId x) = x

getEditTutorialR :: TutorialName -> TutorialNames -> Handler Html
getEditTutorialR tn tns = do
    Entity _ Profile {..} <- requireProfile
    (Entity tid tutorial, entMid@(Entity mid Tmember {..})) <- $runDB $ do
        eres <- followSlugPath profileUser tn tns
        (mid, Entity _ content, _) <- either (const notFound) return eres
        case content of
            TcontentTutorialSum tid -> do
                t <- get404 tid
                m <- get404 mid
                return (Entity tid t, Entity mid m)
            _ -> notFound
    unless (tutorialAuthor tutorial == profileUser) notFound

    app <- getYesod
    ((res, widget), enctype) <- runFormPost $ form entMid tutorial $ appGhcEnvs app
    case res of
        FormSuccess Meta {..} -> do
            mslug <- $runDB $ do
                update tid
                    [ TutorialTitle =. metaTitle
                    , TutorialDesc =. metaDesc
                    , TutorialEnvironment =.
                        case metaEnv of
                            DefaultEnv -> Nothing
                            Env x -> Just x
                    ]
                case () of
                    ()
                        | metaSlug /= tmemberSlug -> do
                            update mid
                                [ TmemberSlug =. metaSlug
                                , TmemberSlugUserGen =. True
                                ]
                            return $ Just metaSlug
                        | metaTitle /= (tutorialTitle tutorial) && not tmemberSlugUserGen -> do
                            slug <- getUniqueSlug tmemberHolder metaTitle
                            if slug /= tmemberSlug
                                then do
                                    update mid [TmemberSlug =. slug]
                                    return $ Just slug
                                else return Nothing
                        | otherwise -> return Nothing
            case mslug of
                Nothing -> return ()
                Just slug ->
                    let (tn', tns') =
                            case reverse tns of
                                [] -> (slug, [])
                                _:rest -> (tn, reverse $ slug : rest)
                     in redirect $ EditTutorialR tn' tns'
        _ -> return ()

    googleAnalytics <- makeGoogleAnalytics Nothing

    isPublished <- fmap isJust $ $runDB $ getBy $ UniquePublishedTutorial tid

    pc <- widgetToPageContent $ do
        setTitle (toHtml (tutorialTitle tutorial))
        defaultWidgetJs
        $(combineStylesheets 'StaticR
            [ codemirror_lib_codemirror_css
            , codemirror_addon_dialog_dialog_css
            , codemirror_addon_hint_show_hint_css
            ])
        $(widgetFile "edit-tutorial")
    withUrlRenderer
        [hamlet|
            $doctype 5
            <html>
                <head>
                    <title>#{pageTitle pc}
                    ^{pageHead pc}
                <body>^{pageBody pc}
        |]

postEditTutorialR :: TutorialName -> TutorialNames -> Handler Html
postEditTutorialR = getEditTutorialR

postPublishTutorialR :: TutorialName -> TutorialNames -> Handler ()
postPublishTutorialR tn tns = do
    Entity _ Profile {..} <- requireProfile
    dest <- $runDB $ do
        eres <- followSlugPath profileUser tn tns
        (_, Entity _ content, _) <- either (const notFound) return eres
        case content of
            TcontentTutorialSum tid -> do
                t@Tutorial {..} <- get404 tid
                unless (tutorialAuthor == profileUser) notFound
                putFrozenTutorial tid t
                update tid [TutorialIsDirty =. False]

                now <- liftIO getCurrentTime

                let desc = if null $ unTextarea tutorialDesc
                                then Textarea $ concat
                                              $ toChunks
                                              $ ellipsize 100
                                              $ plainText
                                              $ markdown def
                                              $ fromChunks
                                              $ return
                                              $ unTutorialContent tutorialContent
                                else tutorialDesc

                shouldPublish <- if length (unTutorialContent tutorialContent) < 100 then return False else do
                    mrt <- getBy $ UniqueRecentTutorial tid
                    let oneDay = 60 * 60 * 24
                    case mrt of
                        Nothing -> return True
                        Just (Entity rtid RecentTutorial {..})
                            | addUTCTime oneDay recentTutorialPublished > now -> do
                                update rtid
                                    [ RecentTutorialTitle =. tutorialTitle
                                    , RecentTutorialDesc =. desc
                                    ]
                                return False
                            | otherwise -> return True

                when shouldPublish $ do
                    deleteWhere [RecentTutorialTutorial ==. tid]
                    insert_ RecentTutorial
                        { recentTutorialTutorial = tid
                        , recentTutorialPublished = now
                        , recentTutorialTitle = tutorialTitle
                        , recentTutorialDesc = desc
                        }

                populateUserSummary profileUser
                setMessage "Tutorial published"
                return $ EditTutorialR tn tns
            _ -> notFound
    redirect dest

postUnpublishTutorialR :: TutorialName -> TutorialNames -> Handler ()
postUnpublishTutorialR tn tns = do
    Entity _ Profile {..} <- requireProfile
    dest <- $runDB $ do
        eres <- followSlugPath profileUser tn tns
        (_, Entity _ content, _) <- either (const notFound) return eres
        case content of
            TcontentTutorialSum tid -> do
                Tutorial {..} <- get404 tid
                unless (tutorialAuthor == profileUser) notFound
                deleteBy $ UniquePublishedTutorial tid
                deleteWhere [RecentTutorialTutorial ==. tid]
                populateUserSummary profileUser
                setMessage "Tutorial unpublished"
                return $ EditTutorialR tn tns
            _ -> notFound
    redirect dest

postSaveTutorialR :: TutorialName -> TutorialNames -> Handler Value
postSaveTutorialR tn tns = do
    Entity _ Profile {..} <- requireProfile
    $runDB $ do
        eres <- followSlugPath profileUser tn tns
        (_, Entity _ content, _) <- either (const notFound) return eres
        case content of
            TcontentTutorialSum tid -> do
                saveTutorial tid profileUser
            _ -> notFound

postSaveTutorialIdR :: TutorialId -> Handler Value
postSaveTutorialIdR tid = do
    Entity _ Profile {..} <- requireProfile
    $runDB $ saveTutorial tid profileUser

-- | Save the given tutorial owned by the given user.
saveTutorial :: TutorialId -> UserId -> YesodDB App Value
saveTutorial tid uid = do
    Tutorial {..} <- get404 tid
    unless (tutorialAuthor == uid) notFound
    token <- runInputPost $ ireq intField "token"
    -- We want to allow for empty content, but not missing content.
    mcontent' <- lookupPostParam "content"
    content' <-
        case mcontent' of
            Nothing -> invalidArgs ["No content provided"]
            Just c -> return c
    if TutorialConcurrentToken' token == tutorialConcurrentToken
        then do
            let token' =
                    let x = token + 1
                     in if x > 100000 || x < 0
                            then 0
                            else x
            update tid
                [ TutorialContent =. TutorialContent' content'
                , TutorialConcurrentToken =. TutorialConcurrentToken' token'
                ]
            return $ object ["new-token" .= token']
        else return $ object ["msg" .= asText "Your content is out of date"]
