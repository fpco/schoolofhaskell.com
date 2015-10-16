module Handler.Github where

import Import
import Blaze.ByteString.Builder (copyByteString, toByteString)

getGithubR :: Handler ()
getGithubR = do
    _ <- requireProfile
    mdest <- lookupGetParam "dest"
    dest <-
        case mdest of
            Nothing -> fmap ($ ProfileR) getUrlRender
            Just dest -> return dest
    getGithubDest dest >>= redirect

getGithubDest :: Text -> Handler Text
getGithubDest redirectUrl = do
    y <- getYesod
    render <- getUrlRender
    return $ decodeUtf8 $ toByteString $
        copyByteString "https://github.com/login/oauth/authorize" ++
        renderQueryBuilder True
            [ ("client_id", Just $ githubClientId y)
            , ("redirect_uri", Just $ encodeUtf8 $ render (GithubCallbackR redirectUrl))
            , ("scope", Just "repo user admin:public_key")
            ] ++ copyByteString "#github"

deleteGithubR :: Handler ()
deleteGithubR = do
    Entity pid _ <- requireProfile
    $runDB $ update pid [ProfileGithubAccessKey =. Nothing]
    setMessage
        [shamlet|
            Github access revoked.
            For your security, you should revoke the authentication token and SSH keys on the
            <a href=https://github.com/settings/applications>GitHub application settings page#
            \.
        |]
    render <- getUrlRender
    redirect $ render ProfileR ++ "#github"
