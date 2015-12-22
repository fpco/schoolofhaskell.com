module Handler.RecentContentFeed where

import Import

getRecentContentFeedR :: Handler TypedContent
getRecentContentFeedR = do
    recents <- getRecents 10 0
    updated <-
        case recents of
            [] -> notFound
            (_, x):_ -> return $ hcWhen x
    newsFeed Feed
        { feedTitle = "Published Content- School of Haskell"
        , feedLinkSelf = RecentContentFeedR
        , feedLinkHome = HomeR
        , feedAuthor = "FP Complete"
        , feedDescription = "Recently published tutorials from the School of Haskell"
        , feedLanguage = "en"
        , feedUpdated = updated
        , feedEntries = map recentToEntry recents
        }
