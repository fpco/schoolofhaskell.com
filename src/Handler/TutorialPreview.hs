module Handler.TutorialPreview where

import Model
import Import
import Yesod.Auth.Email (randomKey)
import Handler.UserTutorial (disqusWidget)

postTutorialPreviewCodeResetR :: TutorialId -> PreviewCode -> Handler ()
postTutorialPreviewCodeResetR tid previewCode = do
    Entity _ author <- requireProfile
    let uid = profileUser author
    mtutorial <- $runDB $ get tid
    case mtutorial of
        Just tutorial | uid == tutorialAuthor tutorial -> do
            $runDB $ do
                previewCode' <- getPreviewCode tid tutorial
                unless (previewCode == previewCode') notFound
                update tid [TutorialPreviewCode =. Nothing]
            setMessage "New tutorial preview URL generated"
            redirect $ TutorialPreviewR tid
        _ -> notFound

getPreviewCode :: TutorialId -> Tutorial -> YesodDB App PreviewCode
getPreviewCode tid Tutorial {..} =
    case tutorialPreviewCode of
        Just pc -> return pc
        Nothing -> do
            pc <- lift getYesod >>= liftIO . randomKey
            update tid [TutorialPreviewCode =. Just pc]
            return pc

getTutorialPreviewR :: TutorialId -> Handler Html
getTutorialPreviewR tid = do
    Entity _ author <- requireProfile
    let uid = profileUser author
    mtutorial <- $runDB $ get tid
    case mtutorial of
        Just tutorial | uid == tutorialAuthor tutorial -> do
            previewCode <- $runDB $ getPreviewCode tid tutorial
            redirect $ TutorialPreviewCodeR tid previewCode
        _ -> notFound

getTutorialPreviewCodeR :: TutorialId -> PreviewCode -> Handler Html
getTutorialPreviewCodeR tid previewCode = do
    mtutorial <- $runDB $ get tid
    tutorial <- maybe notFound return mtutorial
    previewCode' <- $runDB $ getPreviewCode tid tutorial
    unless (previewCode == previewCode') notFound
    Entity _ author <- $runDB $ getBy404 $ UniqueProfile $ tutorialAuthor tutorial
    let profile = author -- For user-tutorial.hamlet
    mcurrentUID <- maybeAuthId

    (header, footer, sidebar) <- getTutorialExtras
    (maside, body) <- displayTutorial $ tutorialContent tutorial
    let title = case tutorialTitle tutorial of
            Title "" -> Title "Untitled"
            x -> x
        widget = do
            setTitle $ toHtml title
            let related = [] :: [(Route App, Text)]
                mkLink = error "mkLink"
                mprev = Nothing
                mnext = Nothing
                mgroup = Nothing :: Maybe ((Route App, [(Text, Text)]), Text)
                relatedContent = $(widgetFile "related-content")
            $(widgetFile "user-tutorial")
        tabber =
                [whamlet|
                    $if mcurrentUID == Just (tutorialAuthor tutorial)
                        <p .alert .alert-block>
                            You are viewing a preview of your tutorial.
                            You can share this preview with others using the URL: #
                            $with url <- TutorialPreviewCodeR tid previewCode
                                <a href=@{url}>@{url}#
                            . You may also reset the preview URL;
                            doing so will prevent people with the current preview URL from viewing the preview any more.
                            <form method=post action=@{TutorialPreviewCodeResetR tid previewCode}>
                                <input type=submit .btn value="Reset preview code">
                    $else
                        <p .alert .alert-block>
                            You are currently viewing a tutorial preview.
                            This is not yet publicly available.
                |]
        top = [whamlet|
            $newline never
            <div .meta>
                <h1 itemprop=name>#{title}
                ^{tabber}
          |]
        bottomFooter = $(widgetFile "creative-commons")
    defaultLayoutExtra (Just top) maside (Just bottomFooter) [] Nothing widget
