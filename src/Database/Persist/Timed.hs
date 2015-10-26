{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}
module Database.Persist.Timed
    ( runDBTimed
    ) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.Lifted
import Control.Exception.Lifted (throwIO)
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.Conduit.Pool
import Database.Persist.Postgresql
import Prelude
import System.Posix.Types (CPid)

withAsyncLifted :: MonadBaseControl IO m
                => m a
                -> m b
                -> m b
withAsyncLifted action inner = control $ \runInIO -> withAsync
    (runInIO action)
    (\_ -> runInIO inner)

runDBTimed :: (MonadBaseControl IO m, MonadLogger m)
           => Pool (CPid, Connection)
           -> Int
           -> Loc
           -> SqlPersistT m a
           -> m a
runDBTimed pool time0 loc action = do
    mres <- withResourceTimeout 2000000 pool $ \(cpid, conn) ->
        withAsyncLifted (reporter cpid time0) $ runSqlConn action conn
    maybe (throwIO Couldn'tGetSQLConnection) return mres
  where
    reporter cpid time = do
        threadDelay time
        monadLoggerLog loc "persistent-timed" LevelWarn $ concat
            [ "Database action took longer than "
            , show time
            , " microseconds"
            , "; PID=="
            , show cpid
            ]
        reporter cpid $ min (time0 * 1000) (time * 2)
