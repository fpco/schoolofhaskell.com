{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Azure.BlobStorage.Exception where

import           Azure.BlobStorage.Request
import           Azure.BlobStorage.Types
import           Azure.BlobStorage.Util
import           Control.Exception
import           Control.Monad.Reader hiding (forM)
import           Control.Monad.Trans.Resource
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8)
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           Text.XML as XML
import           Text.XML.Cursor ((&/), ($/), element)
import qualified Text.XML.Cursor as Cursor
import           Prelude

checkErrorCode :: MonadThrow m => ByteString -> CheckStatus m
checkErrorCode = checkXmlErrorCode . checkErrorCode'

checkErrorCode' :: MonadThrow m => ByteString -> Int -> Text -> Cursor.Cursor -> m ()
checkErrorCode' sig status code cursor = do
  let whenStatus s c = when (status == s && code == c)
  whenStatus 404 "BlobNotFound" $ throwM BlobNotFound
  whenStatus 404 "ContainerNotFound" $ throwM ContainerNotFound
  whenStatus 409 "ContainerAlreadyExists" $ throwM ContainerAlreadyExists
  whenStatus 403 "AuthenticationFailed" $ do
    case cursor $/ element "AuthenticationErrorDetail" &/ Cursor.content of
      [d] -> do
        let msig =
              Text.stripPrefix "The MAC signature found in the HTTP request '" d >>=
              return . snd . Text.break (== '\'') >>=
              Text.stripPrefix "' is not the same as any computed signature. Server used following string to sign: '" >>=
              return . fst . Text.break (== '\'')
        case msig of
          Nothing -> return ()
          Just (unescapeText -> serverSig) ->
            let clientSig = decodeUtf8 sig
            in if clientSig == serverSig
                  then throwM AccountKeyWrong
                  else throwM $ SignatureMismatch { ours = clientSig
                                                 , theirs = serverSig
                                                 }
      _ -> return ()

checkXmlErrorCode :: MonadThrow m => (Int -> Text -> Cursor.Cursor -> m ()) -> CheckStatus m
checkXmlErrorCode f res consumeBody = do
    -- Parse the XML exceptions from error codes.  This intentionally
    -- omits handling of 400 (bad request) errors, as the error
    -- contents likely isn't the XML format we expect (e.g. HTML instead).
    when (code > 400) $ do
      body <- consumeBody
      let invalidXml err = throwM $ InvalidErrorXml err $
            case mfallback >>= fromException of
              Nothing -> Nothing
              Just (StatusCodeException s hs cj) ->
                let hs' = ("X-Response-Body-Start", BS.take 1024 body) : hs
                in Just $ StatusCodeException s hs' cj
              Just ex -> Just ex
      case XML.parseLBS def (LBS.fromChunks [body]) of
        Right (element "Error" . Cursor.fromDocument -> [cursor]) -> do
          case cursor $/ element "Code" &/ Cursor.content of
            [errorCode] -> f code errorCode cursor
            _ -> invalidXml "Expected <Code> element inside <Error>"
        Right _ -> invalidXml "Expected <Error> element"
        Left err -> invalidXml (Text.pack (show err))
    -- Fallback on the default status checking from the default request.
    maybe (return ()) throwM mfallback
  where
    code = statusCode (responseStatus res)
    mfallback = checkStatus
                  def
                  (responseStatus res)
                  (responseHeaders res)
                  (responseCookieJar res)

-- | Throw an exception if the status doesn't indicate the blob was
-- successfully created.
checkCreated :: MonadThrow m => ByteString -> CheckStatus m
checkCreated sig res consumeBody =
  if responseStatus res == Status 201 "Created"
    then return ()
    else do
      checkErrorCode sig res consumeBody
      throwM BlobNotCreated
