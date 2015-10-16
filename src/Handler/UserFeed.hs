module Handler.UserFeed where

import Import

getUserFeedR :: UserHandle -> Handler TypedContent
getUserFeedR uh = do
    Entity _ profile <- $runDB $ profileByHandle uh
    let uid = profileUser profile
    recents <- getRecentsForUser uid 10 0
    updated <-
        case recents of
            [] -> liftIO getCurrentTime
            (_, x):_ -> return $ hcWhen x
    newsFeed Feed
        { feedTitle = unUserHandle uh <> "'s published content"
        , feedLinkSelf = UserFeedR uh
        , feedLinkHome = HomeR
        , feedAuthor = unUserHandle uh
        , feedDescription = "Recently published content by " `mappend` toHtml (unUserHandle uh)
        , feedLanguage = "en"
        , feedUpdated = updated
        , feedEntries = map recentToEntry recents
        }
