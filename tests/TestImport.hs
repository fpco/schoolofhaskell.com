{-# LANGUAGE OverloadedStrings #-}
module TestImport
    ( module Yesod.Test
    , App (..)
    , runDB
    , statusIs
    ) where

import ClassyPrelude
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Data.Conduit.Pool (Pool)
import Database.Persist.Sql hiding (LogFunc, runSqlPool, runSqlPersistMPool)
import Foundation hiding (runDB)
import Import (runSqlPool)
import System.Log.FastLogger (LogStr)
import Yesod.Test hiding (statusIs)
import qualified Yesod.Test
import Network.Wai.Test (simpleHeaders, simpleBody)

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

runSqlPersistMPool :: LogFunc
                   -> SqlPersistT (LoggingT (ResourceT IO)) a
                   -> Pool (ignored, SqlBackend)
                   -> IO a
runSqlPersistMPool logFunc x pool = runResourceT $ runLoggingT (runSqlPool pool x) logFunc

runDB :: SqlPersistT (LoggingT (ResourceT IO)) a -> YesodExample App a
runDB query = do
    pool <- fmap connPool getTestYesod
    liftIO $ runSqlPersistMPool (\_ _ _ _ -> return ()) query pool

statusIs :: Text -> Int -> YesodExample site ()
statusIs src code = Yesod.Test.statusIs code `catch` \e -> do
    mres <- getResponse
    body <-
        case mres of
            Just res
                | Just ctype <- lookup "content-type" (simpleHeaders res)
                , "text/html" `isPrefixOf` ctype -> do
                    errs <- htmlQuery ".main-content > pre"
                    return $ mconcat errs
                | otherwise -> return $ simpleBody res
            Nothing -> return $ "No response available"
    throwIO $ StatusExc src e $ decodeUtf8 $ toStrict body

data StatusExc = StatusExc Text SomeException Text
    deriving (Show, Typeable)
instance Exception StatusExc
