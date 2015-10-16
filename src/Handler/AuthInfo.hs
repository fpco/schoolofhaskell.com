module Handler.AuthInfo where

import Import

getAuthInfoR :: Handler Value
getAuthInfoR = do
    ma <- maybeAuth
    case ma of
        Nothing -> return $ object ["auth" .= False]
        Just (Entity uid User {..}) -> do
            mp <- $runDB $ getBy $ UniqueProfile uid
            let addUsername =
                    case mp of
                        Nothing -> id
                        Just (Entity _ Profile {..}) ->
                            (("username" .= profileHandle):)
            return $ object $ addUsername
                [ "auth" .= True
                , "email" .= userEmail
                ]
