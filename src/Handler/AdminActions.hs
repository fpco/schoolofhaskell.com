module Handler.AdminActions where

import Import
import Handler.AdminUserList (respondCSV)

getAdminActionsR :: Handler TypedContent
getAdminActionsR = respondCSV [] [Asc AdminActionTimestamp] $ \(Entity _ AdminAction {..}) ->
    return $ mapFromList
        [ ("userid", toPathPiece adminActionUser)
        , ("email", fromMaybe "" adminActionEmail)
        , ("timestamp", tshow adminActionTimestamp)
        , ("desc", adminActionDesc)
        ]
