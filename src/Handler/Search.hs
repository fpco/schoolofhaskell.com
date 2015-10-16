module Handler.Search where

import Import

getSearchR :: Handler Html
getSearchR = do
    search <- fromMaybe "" <$> lookupGetParam "search"
    defaultLayout $ do
        setTitle $ toHtml $ "Search: " ++ search
        $(widgetFile "search")
