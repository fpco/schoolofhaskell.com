module Handler.AdminUserList where

import Import
import Blaze.ByteString.Builder (fromByteString)
import qualified Data.Conduit.List as CL
import Data.CSV.Conduit (writeHeaders, fromCSV)

respondCSV :: (PersistEntity entity, PersistEntityBackend entity ~ SqlBackend)
           => [Filter entity]
           -> [SelectOpt entity]
           -> (Entity entity -> YesodDB App (Map Text Text))
           -> Handler TypedContent
respondCSV filts opts toRow = respondSourceDB typePlain
    $  selectSource filts opts
    $= CL.mapM toRow
    $= (writeHeaders def >> fromCSV def)
    $= CL.concatMap (\bs -> [Chunk $ fromByteString bs, Flush])

getAdminUserListR :: Handler TypedContent
getAdminUserListR =
    respondCSV [] [] toRow
  where
    toRow (Entity _ Profile {..}) = do
        User {..} <- get404 profileUser
        return $ mapFromList
            [ ("userid", toPathPiece profileUser)
            , ("email", userEmail)
            , ("username", unUserHandle profileHandle)
            , ("display_name", profileDisplay)
            ]
