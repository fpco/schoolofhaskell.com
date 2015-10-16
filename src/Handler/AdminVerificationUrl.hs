module Handler.AdminVerificationUrl where

import Import
import Yesod.Auth.Email (randomKey)
import Model -- UniqueUser

getAdminVerificationUrlR :: Handler Html
getAdminVerificationUrlR = do
    app <- getYesod
    memail <- runInputGet $ iopt emailField "email"
    mpair <-
        case memail of
            Nothing -> return Nothing
            Just email -> $runDB $ do
                mident <- selectFirst [IdentLident ==. mkLowerCaseText email] []
                key <- liftIO $ randomKey app
                uid <- case mident of
                    Just (Entity _ i) -> do
                        let uid = identUser i
                        update uid [UserVerkey =. Just key]
                        return uid
                    Nothing -> do
                        muser <- getBy $ UniqueUser email
                        case muser of
                            Nothing -> addUnverifiedDB email key
                            Just (Entity uid _) -> do
                                update uid [UserVerkey =. Just key]
                                return uid
                return $ Just (email, VerifyEmailR uid key)
    defaultLayout $ do
        setTitle "Verification URLs"
        [whamlet|
            <h1>Verification URLs
            <p>
                <a href=@{AdminR}>Return to admin page.
            $maybe (email, url) <- mpair
                <p>
                    Verification URL for email address #{email} is
                    <b>@{url}
                    \.
                <p .text-warning>
                    You should only send this URL to the specified email address.
                    Otherwise, a user could be verified for an email address he/she does not control.
            <form>
                Email address:
                <input type=email name=email>
                <input type=submit value="Get verification URL">
        |]
