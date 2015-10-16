module Handler.ImpersonateUser where

import Import

postImpersonateUserR :: Handler ()
postImpersonateUserR = do
    mident <- lookupPostParam "ident"
    eres <-
        case mident of
            Nothing -> return $ Left "No identifier provided"
            Just ident -> do
                eres <- $runDB $ lookupUserNameOrEmail ident
                return $ (, ident) <$> eres
    case eres of
        Left t -> do
            setMessage (toHtml t)
            redirect AdminR
        Right (uid, ident) -> do
            Entity callerId caller <- requireAuth
            now <- liftIO getCurrentTime
            $runDB $ insert_ AdminAction
                { adminActionUser = fromEnum $ toBackendKey callerId
                , adminActionEmail = Just (userEmail caller)
                , adminActionTimestamp = now
                , adminActionDesc = concat
                    [ "Impersonated user "
                    , toPathPiece uid
                    , " identified by "
                    , ident
                    ]
                }
            setSession credsKey $ toPathPiece uid
            setMessage "You have been logged in as a different user. Please be respectful!"
            redirect DashboardR

-- | Copied from Yesod.Auth
credsKey :: Text
credsKey = "_ID"
