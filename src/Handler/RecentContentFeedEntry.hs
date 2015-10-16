module Handler.RecentContentFeedEntry where

import Import

getRecentContentFeedEntryR :: RecentTutorialId -> Handler Html
{-

This code should absolutely be safe, but it causes a GHC segfault. __WE NEED TO
INVESTIGATE WHY__

getRecentContentFeedEntryR rtid = do
    mroute <- $runDB $ do
        RecentTutorial {..} <- get404 rtid
        getCanonicalRoute recentTutorialTutorial
    maybe notFound redirect mroute


-}
getRecentContentFeedEntryR rtid = $runDB $ do
    mrt <- get rtid
    case mrt of
        Nothing -> notFound
        Just RecentTutorial {..} -> do
            x <- getCanonicalRoute recentTutorialTutorial
            case x of
                Nothing -> notFound
                Just route -> redirect route
