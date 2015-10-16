module Handler.VerifyEmail where

import Import
import Yesod.Auth
import Yesod.Auth.Email (registerR, setLoginLinkKey)

getVerifyEmailR :: UserId -> Text -> Handler Html
getVerifyEmailR uid verkey = do
    $runDB $ do
        muser <- get uid
        user <-
            case muser of
                Just user | userVerkey user == Just verkey -> return user
                _ -> lift $ do
                    setMessage "We are sorry, but the link you provided is either invalid or expired. Please enter your email address to generate a new confirmation link."
                    redirect $ AuthR registerR
        setLoginLinkKey uid
        lift $ setCreds False $ Creds "verify" (userEmail user) []
        update uid [UserVerified =. True, UserVerkey =. Nothing]
    render <- getUrlRender
    _ <- requireProfile
    redirectUltDest $ render ProfileR ++ "#change-password"
