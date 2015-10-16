module Import.DBTimed
    ( Import.DBTimed.runDB
    , runDBT
    , PidConnectionPool (..)
    ) where

import Prelude
import Yesod hiding (lift)
import Language.Haskell.TH.Syntax
import Control.Monad.Logger
import Control.Applicative
import Database.Persist.Postgresql
import System.Posix.Types (CPid)
import Data.Conduit.Pool
import qualified Database.Persist.Timed

class (YesodPersist site, YesodPersistBackend site ~ SqlBackend) => PidConnectionPool site where
    pidConnectionPool :: site -> Pool (CPid, SqlBackend)

runDBTimed :: PidConnectionPool site
           => Int
           -> Loc
           -> SqlPersistT (HandlerT site IO) a
           -> HandlerT site IO a
runDBTimed time0 loc action = do
    pool <- pidConnectionPool <$> getYesod
    Database.Persist.Timed.runDBTimed pool time0 loc action

runDB :: Q Exp
runDB = do
    loc <- qLocation
    [|runDBTimed 200000 $(liftLoc loc)|]

runDBT :: Q Exp
runDBT = Import.DBTimed.runDB
