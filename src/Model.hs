{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model
    ( module Model
    , module Model.Types
    ) where

import           ClassyPrelude.Yesod hiding (show)
import           Database.Persist.Quasi
import           Model.Types

-- You can define all of your database entities in the entities file.  You can
-- find more information on persistent and how to declare entities at:
-- http://www.yesodweb.com/book/persistent/
share [ mkPersist sqlSettings
      , mkMigrate "migrateAll"
      , mkDeleteCascade sqlSettings
      ]
    $(persistFileWith lowerCaseSettings "config/models")
