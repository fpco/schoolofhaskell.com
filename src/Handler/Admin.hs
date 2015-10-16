module Handler.Admin where

import Import

getAdminR :: Handler Html
getAdminR = do
    (activated, totalTutorials, publishedTutorials) <-
        $runDB $ (,,)
            <$> count ([] :: [Filter Profile])
            <*> count ([] :: [Filter Tutorial])
            <*> count ([] :: [Filter PublishedTutorial])
    defaultLayout $ do
        setTitle "Admin Page"
        $(widgetFile "admin")
