module Handler.Sitemap
    ( getRobotsR
    , getSitemapR
    ) where

import Import
import Yesod.Sitemap
import qualified Data.Conduit.List as CL

getRobotsR :: Handler RepPlain
getRobotsR = do
    app <- getYesod
    txts <-
        if appPrivate app
            then return
                [ "User-agent: *"
                , "Disallow: /"
                ]
            else do
                ur <- getUrlRender
                return
                    [ "Sitemap: " ++ ur SitemapR
                    , "User-agent: *"
                    , "Disallow: /tutorial-raw/"
                    , "Disallow: /hoogle"
                    ]
    return $ RepPlain $ toContent $ unlines txts

getSitemapR :: Handler TypedContent
getSitemapR =
    sitemap $ runDBSource $ do
        yield $ SitemapUrl HomeR Nothing (Just Daily) (Just 1.0)
        yield $ SitemapUrl UsersR Nothing (Just Daily) (Just 0.6)
        yield $ SitemapUrl RecentContentR Nothing (Just Daily) (Just 0.6)
        selectSource [] [] $= CL.mapMaybeM (\(Entity _ Profile {..}) -> do
            mus <- getBy $ UniqueUserSummary profileHandle
            case mus of
                Just (Entity _ us) | userSummaryTutcount us > 0 -> return $ Just $
                    SitemapUrl (UserR profileHandle) Nothing (Just Weekly) (Just 0.5)
                _ -> return Nothing
                )
        selectKeys [] [] $= CL.mapMaybeM (fmap (fmap goTutorial) . getCanonicalRoute)
  where
    goTutorial route = SitemapUrl route Nothing (Just Monthly) (Just 0.6)
