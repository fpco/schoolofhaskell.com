module Import.Migration
    ( MigrationSettings (..)
    , performPreMigrations
    , performPostMigrations
    ) where

import Control.Monad.Logger (LoggingT (..))
-- import Database.Persist.Sql (rawExecute, rawQuery)
import Import

data MigrationSettings = MigrationSettings

type M = SqlPersistT (LoggingT (ResourceT IO))

-- Previously used migration keys:
--
-- 1-29: old site migrations

-- | Migrations performed /before/ the Persistent migration.
performPreMigrations :: MigrationSettings -> M ()
performPreMigrations _ms = return ()

-- | Migrations performed /after/ the Persistent migration.
performPostMigrations :: MigrationSettings -> App -> M ()
performPostMigrations _ms _app = return ()

{-
maybeDoMigration :: MigrationSettings -> Int -> M () -> M ()
maybeDoMigration _ms ix migrate = do
    mx <- getBy $ UniqueMigrationPerformed ix
    case mx of
        Nothing -> do
            $logWarn $ "Starting migration: " ++ tshow ix
            migrate
            insert_ $ MigrationPerformed ix
            snapshot
            $logWarn $ "Completed migration: " ++ tshow ix
        Just _ -> return ()

snapshot :: M ()
snapshot = do
    rawExecute "COMMIT" []
    rawExecute "BEGIN" []

doesTableExist :: (MonadResource m, MonadLogger m) => Text -> SqlPersistT m Bool
doesTableExist name =
    rawQuery sql [PersistText name] $$ start
  where
    sql = "SELECT COUNT(*) FROM information_schema.tables WHERE table_name=?"
    start = await >>= maybe (error "No results when checking doesTableExist") start'
    start' [PersistInt64 0] = finish False
    start' [PersistInt64 1] = finish True
    start' res = error $ "doesTableExist returned unexpected result: " ++ show res
    finish x = await >>= maybe (return x) (error "Too many rows returned in doesTableExist")

doesColumnExist :: (MonadResource m, MonadLogger m) => Text -> Text -> SqlPersistT m Bool
doesColumnExist table column =
    rawQuery sql [PersistText table, PersistText column] $$ start
  where
    sql = "SELECT COUNT(*) FROM information_schema.columns WHERE table_name=? and column_name=?"
    start = await >>= maybe (error "No results when checking doesTableExist") start'
    start' [PersistInt64 0] = finish False
    start' [PersistInt64 1] = finish True
    start' res = error $ "doesColumnExist returned unexpected result: " ++ show res
    finish x = await >>= maybe (return x) (error "Too many rows returned in doesColumnExist")
-}
