module Handler.ContentProxy where

import Import
import Network.Wai.Conduit (responseSource)
import qualified Network.Wai as Wai
import Blaze.ByteString.Builder (fromByteString)
import Data.CaseInsensitive (CI)
import qualified Data.ByteString as S
import FP.EnvSettings (learningSiteApproot)

-- | Creates a proxy for user content, allowing us to avoid insecure content on
-- https pages.
getContentProxyR :: Handler Html
getContentProxyR = do
    -- The purpose of this line is to discourage usage of our proxy feature for
    -- hotlinking from other sites.
    wr <- waiRequest
    case lookup "referer" $ Wai.requestHeaders wr of
        Nothing -> return ()
        Just referer ->
            unless (encodeUtf8 learningSiteApproot `S.isPrefixOf` referer) notFound

    msrc <- lookupGetParam "src"
    case msrc of
        Nothing -> notFound
        Just url -> do
            req <- liftIO $ parseUrl $ unpack url
            (_, res) <- acquireResponse req >>= allocateAcquire
            -- Protect against an XSS attack #3989
            if (maybe True (not . isImage) $ lookup "content-type" $ responseHeaders res)
              then defaultLayout $ do
                    let content = "Content proxy for non-images disallowed" :: Html
                    setTitle content
                    [whamlet|<p>#{content}|]
              else sendWaiResponse $ responseSource
                (responseStatus res)
                (filter ((`member` safeResponseHeaders) . fst) $ responseHeaders res)
                (mapOutput (Chunk . fromByteString) $ responseBody res)
  where
    isImage = isPrefixOf "image/"

safeResponseHeaders :: HashSet (CI ByteString)
safeResponseHeaders = setFromList
    [ "content-type"
    , "content-length"
    , "etag"
    , "expires"
    , "last-modified"
    ]
