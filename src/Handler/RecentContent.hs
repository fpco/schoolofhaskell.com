module Handler.RecentContent where

import Import

perPage :: Int
perPage = 10

addEmptyParams :: HolderContent UTCTime (Route App)-> HolderContent (Maybe UTCTime) (Route App, [(Text, Text)])
addEmptyParams hc = hc
    { hcURL = (hcURL hc, [])
    , hcWhen = Just (hcWhen hc)
    , hcForeignDeleteURL = Nothing
    }

getRecentContentR :: Handler Html
getRecentContentR = do
    mpageT <- lookupGetParam "page"
    let page = max 1 $ fromMaybe 1 $ mpageT >>= readMay
        offset = (page - 1) * perPage
    total <- $runDB $ count ([] :: [Filter RecentTutorial])
    let pages = max 1 $ ((total - 1) `div` perPage) + 1
    contents <- map (addEmptyParams . snd) <$> getRecents perPage offset
    let mprev = if page == 1 then Nothing else Just (RecentContentR, [("page", tshow $ page - 1)])
        mnext = if page == pages then Nothing else Just (RecentContentR, [("page", tshow $ page + 1)])
    let mprofile = Nothing
    (tutHeader, tutFooter, _tutSidebar) <- getTutorialExtras
    let groupContents = $(widgetFile "group-contents")
        maside = Nothing :: Maybe Widget
        maybeNoContent = Nothing :: Maybe Widget
        displayGravatars = True
    defaultLayout $ do
        let title = "Published Content" :: Html
        setTitle title
        $(widgetFile "recent-content")
