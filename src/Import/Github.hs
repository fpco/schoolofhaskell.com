module Import.Github
    ( revokeGithubAccess
    , deleteGithubSsh
    , createPublicKey
    , InvalidGithubResponse (..)
    ) where

import Foundation
import ClassyPrelude.Yesod
import qualified Data.Aeson
import Data.Conduit.Attoparsec (sinkParser)
import Data.Aeson.Parser (json)
import Control.Monad.Catch.Pure (runCatchT)
import Data.Aeson.Types (parseMaybe)

revokeGithubAccess :: GithubAccessKey -> Entity Profile -> YesodDB App ()
revokeGithubAccess key (Entity pid p) = do
    $logDebug "entered revokeGithubAccess"
    lift $ deleteGithubSsh key (Entity pid p)
    case profileGithubAccessKey p of
        Nothing -> $logDebug "no access key"
        Just access@(GithubAccessKey token) -> ignoreExceptions $ lift $ do
            y <- getYesod
            req' <- liftIO $ parseUrl $ unpack $ decodeUtf8 $ concat
                ["https://api.github.com/applications/"
                , githubClientId y
                , "/tokens/"
                , token
                ]
            let req = applyBasicAuth (githubClientId y) (githubClientSecret y) req'
                    { requestHeaders = [("User-Agent", "FPComplete-soh-site/1.0")]
                    }
            withResponse req $ \res -> do
                let src = responseBody res
                eval <- runCatchT $ transPipe lift src $$ sinkParser json
                case eval of
                    Right (Object o) | Just id' <- parseMaybe (.: "id") o -> do
                        $logDebug $ tshow (id' :: Integer)
                        $logDebug $ tshow $ lookup "url" o
                        -- FIXME following call still fails, don't know why
                        res2 <- performGithub access "DELETE" ("authorizations/" ++ show id') Nothing
                        $logDebug $ tshow res2
                    _ -> return ()
    update pid [ProfileGithubAccessKey =. Nothing]
  where
    ignoreExceptions = handle (\(e :: SomeException) -> $logDebug $ tshow e)

deleteGithubSsh :: GithubAccessKey -> Entity Profile -> Handler ()
deleteGithubSsh key (Entity _ p) = do
    case profileSshKeyPair p of
        Just keypair -> do
            ks <- getKeys
            case lookup (unwords $ take 2 $ words $ decodeUtf8 $ publicKey keypair) ks of
                Nothing -> return ()
                Just id' -> void $ performGithub
                    key
                    "DELETE"
                    ("user/keys/" ++ show (id' :: Integer))
                    Nothing
        Nothing -> return ()
  where
    getKeys = do
        eres <- performGithub key "GET" "user/keys" Nothing
        case eres of
            Right (_, Just (Array x)) -> return $ mapMaybe toKey $ unpack x
            _ -> return []
    toKey (Object o) = do
        i <- parseMaybe (.: "id") o
        String k <- lookup "key" o
        Just (k, i)
    toKey _ = Nothing

performGithub :: GithubAccessKey
              -> Method
              -> [Char] -- ^ path suffix, excluding slash
              -> Maybe Value -- ^ request body
              -> Handler (Either SomeException (Status, Maybe Value))
performGithub (GithubAccessKey token) m suffix mbody = do
    $logDebug $ "Github request to: " ++ tshow suffix
    req' <- liftIO $ parseUrl $ "https://api.github.com/" ++ suffix
    let req = req'
            { requestHeaders = [ ("Authorization", "token " ++ token)
                               , ("User-Agent", "FPComplete-soh-site/1.0") ]
            , checkStatus = \_ _ _ -> Nothing
            , requestBody =
                case mbody of
                    Nothing -> RequestBodyBS mempty
                    Just body -> RequestBodyLBS $ Data.Aeson.encode body
            , method = m
            }
    try $ withResponse req $ \res -> do
        eval <- runCatchT $ responseBody res $$ sinkParser json
        return (responseStatus res, either (const Nothing) Just eval)

createPublicKey :: GithubAccessKey -> SSHKeyPair -> Handler Bool
createPublicKey key SSHKeyPair {..} = do
    eres <- performGithub key "POST" "user/keys" $ Just $ object
        [ "title" .= asText "School of Haskell"
        , "key" .= decodeUtf8 publicKey
        ]
    case eres of
        Left e -> throwIO $ Couldn'tCreatePublicKey $ Just e
        Right (s, _) ->
            case statusCode s of
                201 -> return False
                422 -> return True -- key already exists
                _ -> do
                    $logError $ "Unexpected response from Github: " ++ tshow eres
                    throwIO $ Couldn'tCreatePublicKey Nothing

data InvalidGithubResponse = InvalidGithubResponse SimpleQuery
                           | Couldn'tCreatePublicKey (Maybe SomeException)
    deriving (Show, Typeable)
instance Exception InvalidGithubResponse
