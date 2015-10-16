{-# OPTIONS -Wall #-}
module Database.Persist.Timed.Postgresql
    ( createPidConnectionPool
    ) where

import           Control.Concurrent.MVar.Lifted      (readMVar)
import           Data.Conduit.Pool                   (createPool, Pool)
import           Database.Persist.Postgresql         (connClose, openSimpleConn)
import           Database.Persist.Sql                (LogFunc, Connection)
import           Database.PostgreSQL.LibPQ           (backendPID)
import qualified Database.PostgreSQL.Simple          as PG
import           Database.PostgreSQL.Simple.Internal (connectionHandle)
import           Data.ByteString                     (ByteString)
import           Prelude
import           System.Posix.Types                  (CPid)

createPidConnectionPool :: LogFunc -> ByteString -> Int -> IO (Pool (CPid, Connection))
createPidConnectionPool logFunc pgConnStr pgPoolSize = do
    let mkConn = do
            connRaw <- PG.connectPostgreSQL pgConnStr
            connRaw' <- readMVar $ connectionHandle connRaw
            cpid <- backendPID connRaw'
            conn <- openSimpleConn logFunc connRaw
            return (cpid, conn)
    createPool mkConn (connClose . snd) 1 20 pgPoolSize
