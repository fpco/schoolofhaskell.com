module Handler.GithubCallback where

import Import

getGithubCallbackR :: Text -> Handler Html
getGithubCallbackR redirectUrl = do
    profent@(Entity pid _) <- requireProfile
    y <- getYesod
    mcode <- lookupGetParam "code"
    code <-
        case mcode of
            Nothing -> do
                merr <- lookupGetParam "error"
                invalidArgs $ return $
                    case merr of
                        Just "access_denied" -> "The operation could not complete because Github request was cancelled by the user."
                        _ -> "The Github request did not succeed (expected GET parameter: code, but it wasn't present)"
            Just code -> return code
    req' <- liftIO $ parseUrl "https://github.com/login/oauth/access_token"
    let req = req'
            { queryString = renderSimpleQuery False
                [ ("client_id", githubClientId y)
                , ("client_secret", githubClientSecret y)
                , ("code", encodeUtf8 code)
                ]
            }
    res <- httpLbs req
    let query = parseSimpleQuery $ concat $ toChunks $ responseBody res
    case lookup "access_token" query of
        Nothing -> liftIO $ throwIO $ InvalidGithubResponse query
        Just token -> do
            keypair <- $runDB $ do
                update pid [ProfileGithubAccessKey =. Just (GithubAccessKey token)]
                getSSHKeyPair profent
            b <- createPublicKey (GithubAccessKey token) keypair
            setMessage $
                if b
                    then "Github access granted, public key was already present"
                    else "Github access granted and public key added"
            redirectUltDest redirectUrl
