module Import.Prune
    ( pruneTutorials
    , removeContent
    , removeTutorial
    , runSqlPool
    ) where

import           ClassyPrelude.Yesod hiding (div)
import           Control.Monad.Logger (LoggingT)
import           Data.Conduit.Pool (Pool, withResourceTimeout)
import           Database.Persist.Sql (runSqlConn, PersistentSqlException (Couldn'tGetSQLConnection))
import           Model

runSqlPool :: MonadBaseControl IO m => Pool (ignored, SqlBackend) -> SqlPersistT m a -> m a
runSqlPool pconn r = do
    mres <- withResourceTimeout 2000000 pconn $ runSqlConn r . snd
    maybe (throwIO Couldn'tGetSQLConnection) return mres

pruneTutorials :: Pool (ignored, SqlBackend) -> ResourceT (LoggingT IO) ()
pruneTutorials pool = do
    let runAndCatch name f = runSqlPool pool f `catchAny` \err ->
            $logErrorS "PRUNING" ("Pruning of " <> name <> " failed: " <> tshow err)
    $logDebugS "PRUNING" "Pruning tutorials"
    tutorials <- runSqlPool pool $ selectKeysList [] []
    mapM_ (\x -> runAndCatch ("tutorial " <> toPathPiece x) (removeUnusedTutorial x)) tutorials

unlessUsed :: (PersistEntityBackend value ~ SqlBackend, PersistEntity value, MonadIO m)
           => (Key value -> Unique Tcontent)
           -> Key value
           -> SqlPersistT m ()
           -> SqlPersistT m ()
unlessUsed con id' f = do
    mcontent <- getBy $ con id'
    case mcontent of
        Nothing -> f
        Just (Entity cid _) -> do
            useCount <- count [TmemberContent ==. cid]
            if useCount == 0
                then removeContent cid >> f
                else return ()

removeContent :: MonadIO m => TcontentId -> SqlPersistT m ()
removeContent cid = do
    deleteWhere [ContentLocationContent ==. cid]
    deleteWhere [ContentLikeContent ==. cid]
    delete cid

removeUnusedTutorial :: (MonadLogger m, MonadIO m) => TutorialId -> SqlPersistT m ()
removeUnusedTutorial tid = unlessUsed UniqueContentTutorial tid $ do
    $logInfoS "PRUNING" $ "Pruning tutorial: " ++ toPathPiece tid
    removeTutorial tid

removeTutorial :: MonadIO m => TutorialId -> SqlPersistT m ()
removeTutorial tid = do
    -- Avoiding usage of deleteCascade here to be certain we never accidentally
    -- delete data
    deleteWhere [PublishedTutorialTutorial ==. tid]
    deleteWhere [RecentTutorialTutorial ==. tid]
    delete tid
