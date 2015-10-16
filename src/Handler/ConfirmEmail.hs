module Handler.ConfirmEmail where

import Import

getConfirmEmailR :: ConfirmEmailId -> Text -> Handler ()
getConfirmEmailR ceid key = do
    uid <- requireAuthId
    now <- liftIO getCurrentTime
    $runDB $ do
        ConfirmEmail {..} <- get404 ceid
        unless (confirmEmailVerkey == key) $ lift notFound
        unless (confirmEmailUser == uid) $ lift notFound
        delete ceid
        if (now > confirmEmailExpires)
            then setMessage "This confirmation code has expired, please try again"
            else do
                miid <- insertUnique Ident
                    { identUser = uid
                    , identIdent = unLowerCaseText confirmEmailEmail
                    , identLident = confirmEmailEmail
                    , identEmail = True
                    }
                case miid of
                    Nothing -> setMessage "That email address is already in use"
                    Just _ -> setMessage "Email address confirmed"
    redirect ProfileR
