{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module FP.Extra where

import           ClassyPrelude.Yesod (mconcat, (++), toBuilder, (=$), toStrict, builderToLazy, intersperse)
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Reader (ask)
import           Data.Acquire (with)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Conduit (($$))
import qualified Data.Conduit.List as CL
import           Database.Persist.Sql (rawExecute, rawQueryRes, PersistValue (PersistText), SqlPersistT, connEscapeName, DBName (DBName))
import           Prelude hiding ((++))
import           System.Posix.Files (setFileMode)
import qualified System.Posix.IO as POSIX (fdWrite, closeFd, createFile)

writePrivateKey :: FilePath      -- ^ Path to write the private key
                -> FilePath      -- ^ Path to write the ssh wrapper script
                -> ByteString    -- ^ Contents of the private key
                -> IO ()
writePrivateKey keyFile sshCmd privateKey = do
    B.writeFile keyFile privateKey
    setFileMode keyFile 0o600
    void $ bracket
        (POSIX.createFile sshCmd 0o700)
        POSIX.closeFd
        $ \fd -> POSIX.fdWrite fd $
                 concat [ "#!/bin/sh\nssh -i "
                        , keyFile
                        , " -o UserKnownHostsFile=/dev/null "
                        , "-o StrictHostKeyChecking=no "
                        , " \"$@\"\n" ]

-- | Wipe out all tables in a PostgreSQL database. Obviously this is a very
-- dangerous action!
truncatePostgresqlTables :: (MonadIO m, MonadBaseControl IO m) => SqlPersistT m ()
truncatePostgresqlTables = do
    conn <- ask
    srcRes <- rawQueryRes "select tablename from pg_tables where schemaname='public'" []
    names <- with srcRes
          ($$ CL.mapMaybe getName
          =$ CL.map (toBuilder . connEscapeName conn . DBName)
          =$ CL.consume)
    unless (null names) $ do
        let sql = toStrict $ builderToLazy $
                "TRUNCATE TABLE " ++
                mconcat (intersperse ", " names) ++
                " CASCADE"
        rawExecute sql []
  where
    getName [PersistText name] = Just name
    getName _ = Nothing
