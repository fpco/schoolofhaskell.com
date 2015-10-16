module Import.Recent where

import           ClassyPrelude.Yesod hiding (runDB)
import qualified Data.Conduit.List as CL
import qualified Database.Esqueleto as E
import           Foundation
import           Import.Tutorial (memberToURL, HolderContent (..), MemberType (..))

toRecent :: Entity RecentTutorial
         -> YesodDB App (Maybe (RecentTutorialId, HolderContent UTCTime (Route App)))
toRecent (Entity rtid RecentTutorial {..}) = do
    Tutorial {..} <- get404 recentTutorialTutorial
    mp <- getBy $ UniqueProfile tutorialAuthor
    user <- get404 tutorialAuthor

    mroute <- getCanonicalRoute recentTutorialTutorial

    case (mroute, mp) of
        (Nothing, _) -> do
            delete rtid
            return Nothing
        (Just r, Just (Entity _ profile)) -> return $ Just (rtid, HolderContent
            { hcTitle = recentTutorialTitle
            , hcDesc = recentTutorialDesc
            , hcUser = user
            , hcProfile = profile
            , hcURL = r
            , hcWhen = recentTutorialPublished
            , hcType = MTTutorial
            , hcSlug = Nothing
            , hcPublic = True
            , hcForeignDeleteURL = Nothing
            })
        (_, Nothing) -> do
            $logWarn $ "Missing profile for recent submission: " ++ tshow rtid
            return Nothing

getCanonicalRoute :: (MonadResource m, MonadLogger m) => TutorialId -> SqlPersistT m (Maybe (Route App))
getCanonicalRoute tid = do
    mcontent <- getBy $ UniqueContentTutorial tid
    maybe (return Nothing) (getCanonicalRouteContent . entityKey) mcontent

getCanonicalRouteContent :: (MonadResource m, MonadLogger m) => TcontentId -> SqlPersistT m (Maybe (Route App))
getCanonicalRouteContent cid = do
    members <- selectList [TmemberContent ==. cid] []
    routes <- concat <$> mapM (memberToURL . entityVal) members
    case sortBy (comparing renderRoute) routes of
        [] -> return Nothing
        r:_ ->
            case filter isSchool routes of
                x:_ -> return $ Just x
                [] -> return $ Just r
  where
    isSchool SchoolTutorialR{} = True
    isSchool _ = False

getRecents :: Int -- ^ count
           -> Int -- ^ offset
           -> Handler [(RecentTutorialId, HolderContent UTCTime (Route App))]
getRecents count' offset = $runDB
    $ selectSource [] [Desc RecentTutorialPublished, LimitTo count', OffsetBy offset]
    $$ CL.mapMaybeM toRecent
    =$ CL.consume

getRecentsForUser :: UserId
                  -> Int -- ^ count
                  -> Int -- ^ offset
                  -> Handler [(RecentTutorialId, HolderContent UTCTime (Route App))]
getRecentsForUser uid count' offset =  $runDB $ do
    results <- E.select $ E.from $ \(tut, recent) -> do
        E.where_ $
            (recent E.^. RecentTutorialTutorial) E.==. (tut E.^. TutorialId) E.&&.
            (tut E.^. TutorialAuthor) E.==. E.val uid
        E.orderBy [E.desc (recent E.^. RecentTutorialPublished)]
        E.limit (fromIntegral count')
        E.offset (fromIntegral offset)
        return recent
    fmap catMaybes $ mapM toRecent results

recentToEntry :: (RecentTutorialId, HolderContent UTCTime (Route App))
              -> FeedEntry (Route App)
recentToEntry (rtid, HolderContent {..}) =
    FeedEntry
        { feedEntryLink = RecentContentFeedEntryR rtid
        , feedEntryUpdated = hcWhen
        , feedEntryTitle = unTitle hcTitle
        , feedEntryContent = [shamlet|
            <p>By #{prettyProfile hcProfile}
            \#{hcDesc}
            |]
        }
