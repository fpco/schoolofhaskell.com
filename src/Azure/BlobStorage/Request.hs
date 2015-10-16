{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Azure.BlobStorage.Request where

import           Azure.BlobStorage.Types
import           Azure.BlobStorage.Util
import qualified Blaze.ByteString.Builder as Blaze
import           Control.Applicative
import           Control.Monad.Reader hiding (forM)
import           Control.Monad.Trans.Resource
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import           Data.Conduit
import           Data.Conduit.Binary (sinkLbs)
import           Data.Function (on)
import           Data.Int (Int64)
import           Data.List (sortBy, groupBy, find)
import           Data.Monoid
import           Data.Ord (comparing)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           Prelude
import           Text.XML as XML

emptyRequestBody :: RequestBody
emptyRequestBody = RequestBodyBS ""

requestBodyLength :: MonadThrow m => RequestBody -> m Int64
requestBodyLength (RequestBodyBS x) = return $ fromIntegral $ BS.length x
requestBodyLength (RequestBodyLBS x) = return $ LBS.length x
requestBodyLength (RequestBodyBuilder l _) = return l
requestBodyLength (RequestBodyStream l _) = return l
requestBodyLength (RequestBodyStreamChunked _) = throwM RequestBodyStreamChunkedNotSupported

-- Building and running requests

data AzureRequest m = AzureRequest
  { arRequest :: Request
  -- NOTE: If the ByteString action is used, then an error must be
  -- thrown.  Otherwise, streaming results might be consumed.
  , arCheck :: CheckStatus m
  }

type CheckStatus m = forall a. Response a -> m ByteString -> m ()

runRequest :: MonadResource m
           => BlobStorage
           -> AzureRequest m
           -> m (ResumableSource m ByteString)
runRequest bs ar = do
  manager <- liftIO $ maybe (newManager tlsManagerSettings) return (bsManager bs)
  res <- http (arRequest ar) manager
  arCheck ar res (fmap toStrict (responseBody res $$+- sinkLbs))
  return $ responseBody res

buildRequest :: (Functor m, MonadIO m, MonadThrow m)
             => BlobStorage
             -> Method
             -> [Text]
             -> QueryText
             -> [Header]
             -> RequestBody
             -> (ByteString -> CheckStatus m)
             -> m (AzureRequest m)
buildRequest (BlobStorage {..}) mthd pathSegs query headers body cs = do
  time <- encodeUtf8 . Text.pack <$> liftIO rfc1123Time
  bodyLength <- requestBodyLength body
  let atomType = "application/atom+xml"
      headers' = [ (hContentType, atomType)
                 , (hAccept,      atomType <> ",application/xml")
                 , ("x-ms-date",  time)
                 ] ++ versionHeaders ++ headers
      rpath = bsPathPrefix <> Blaze.toByteString (encodePathSegments pathSegs)
      rquery = Blaze.toByteString $ renderQueryText True query
      AccountName accountName = bsAccountName
      sig = signatureString mthd headers' bodyLength bsAccountName rpath query
      encryptedSig = Base64.encode $ hmacSha256 bsAccountKey sig
      auth = BS.concat ["SharedKey ", accountName,  ":", encryptedSig]
      req = def { host = bsHost
                , port = bsPort
                , secure = bsSecure
                , method = mthd
                , path = rpath
                , queryString = rquery
                , requestBody = body
                , requestHeaders = (hAuthorization, auth) : headers'
                , checkStatus = \_ _ _ -> Nothing
                }
  return $ AzureRequest req (cs sig)

versionHeaders :: [Header]
versionHeaders =
  [ ("x-ms-version",          "2013-08-15")
  , ("DataServiceVersion",    "3.0;NetFx")
  , ("MaxDataServiceVersion", "3.0;NetFx")
  ]

signatureString :: Method
                -> RequestHeaders
                -> Int64
                -> AccountName
                -> ByteString
                -> QueryText
                -> ByteString
signatureString method headers bodyLength accountName rpath rquery =
  BS.intercalate "\n"
    [ method
    , header "Content-Encoding"
    , header "Content-Language"
    -- "Content-Length"
    , if method `elem` ["PUT", "DELETE"] || bodyLength > 0
         then Char8.pack (show bodyLength)
         else ""
    , header "Content-MD5"
    , header "Content-Type"
    , header "Date"
    , header "If-Modified-Since"
    , header "If-Match"
    , header "If-None-Match"
    , header "If-Unmodified-Since"
    , header "Range"
    , canonicalizedHeaders headers <>
      canonicalizedResource accountName rpath rquery
    ]
  where
    header = maybe "" snd . (\name -> find ((name ==) . fst) headers) . CI.mk

-- Based on the Microsoft documentation
-- Title: "Authentication for the Azure Storage Services"
-- Section: "Constructing the Canonicalized Headers String"
-- http://msdn.microsoft.com/en-us/library/azure/dd179428.aspx#Constructing_Element
--
-- (this is where the text in the comments come from)
canonicalizedHeaders :: RequestHeaders -> ByteString
canonicalizedHeaders =
  -- NOTE: I'm a bit dubious about the wording of these next couple
  -- items.  However, this implementation seems to work.
  --
  -- 4. Unfold the string by replacing any breaking white space with a
  -- single space.
  --
  -- 5. Trim any white space around the colon in the header.
  --
  -- 6. Finally, append a new line character to each canonicalized
  -- header in the resulting list. Construct the CanonicalizedHeaders
  -- string by concatenating all headers in this list into a single
  -- string.
  BS.concat . concatMap (\(n, v) -> [strip n, ":", strip v, "\n"]) .
  -- 3. Sort the headers lexicographically by header name, in
  -- ascending order. Note that each header may appear only once in
  -- the string.
  map head .
  groupBy ((==) `on` fst) .
  sortBy (comparing fst) .
  -- 2. Convert each HTTP header name to lowercase.
  map (\(name, body) -> (asciiLowercase (CI.original name), body)) .
  -- 1. Retrieve all headers for the resource that begin with x-ms-,
  -- including the x-ms-date header.
  filter (\(name, _) -> "x-ms-" `BS.isPrefixOf` CI.original name)

-- Based on the Microsoft documentation
-- Title: "Authentication for the Azure Storage Services"
-- Section: "Constructing the Canonicalized Resource String"
-- http://msdn.microsoft.com/en-us/library/azure/dd179428.aspx#Constructing_Element
--
-- (this is where the numbered text in the comments come from)
canonicalizedResource :: AccountName -> ByteString -> QueryText -> ByteString
canonicalizedResource (AccountName accountName) rpath query =
    -- 9. Append a new line character (\n) after each name-value pair.
    --
    -- FIXME(MGS): The examples in the documentation make this seem
    -- more like interpolation.
    BS.intercalate "\n" (path : params)
  where
    -- 1. Beginning with an empty string (""), append a forward slash
    -- (/), followed by the name of the account that owns the resource
    -- being accessed.
    --
    -- 2. Append the resource's encoded URI path, without any query
    -- parameters.
    path = "/" <> accountName <> rpath
    -- 7. Append each query parameter name and value to the string in
    -- the following format, making sure to include the colon (:)
    -- between the name and the value: parameter-name:parameter-value
    --
    -- 8. If a query parameter has more than one value, sort all
    -- values lexicographically, then include them in a
    -- comma-separated list:
    -- parameter-name:parameter-value-1,parameter-value-2,parameter-value-n
    params
      = map (\vs@((k, _):_) ->
          encodeUtf8 k <>
          ":" <>
          BS.intercalate "," (map (maybe "" encodeUtf8 . snd) vs))
      $ groupBy ((==) `on` fst)
      -- 5. Sort the query parameters lexicographically by parameter
      -- name, in ascending order.
      $ sortBy (comparing fst)
      -- 3. Retrieve all query parameters on the resource URI,
      -- including the comp parameter if it exists.
      --
      -- 4. Convert all parameter names to lowercase.
      --
      -- 6. URL-decode each query parameter name and value.
      $ map (\(name, val) -> (Text.toLower name, val)) query
