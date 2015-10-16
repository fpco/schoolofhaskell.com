{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A barebones api for Azure blob storage.
module Azure.BlobStorage
  ( storageEmulator
  , blobStorageFromUrl
  -- * Blob commands
  , getBlob
  , getBlobBytes
  , deleteBlob
  , putBlob
  , putBlobBytes
  , listBlobs
  , listBlobsSource
  -- * Internal blob commands
  -- | (used by 'putBlob' / 'putBlobBytes')
  , putBlock
  , putBlockList
  -- * Types
  , BlobStorage (..)
  , AccountName (..)
  , ContainerID (..)
  , BlobID (..)
  , BlockID (..)
  -- ** Storage account access key
  , SharedKey
  , parseSharedKey
  , parseSharedKeyOrFail
  -- ** Exceptions
  , BlobStorageException (..)
  ) where

import           Azure.BlobStorage.Account
import           Azure.BlobStorage.Exception
import           Azure.BlobStorage.Request
import           Azure.BlobStorage.Types
import           Azure.BlobStorage.Util
import           Control.Applicative
import           Control.Monad.Reader hiding (forM)
import           Control.Monad.Trans.Resource
import           Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI
import           Data.Conduit
import           Data.Conduit.Binary (sinkLbs)
import qualified Data.Conduit.List as CL
import           Data.Int (Int64)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8)
import           Data.Traversable (forM)
import qualified Data.XML.Types as XML
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           Text.XML as XML
import qualified Text.XML.Stream.Parse as XML
import           Prelude

--TODO:
--
-- * Add a listBlob variant that doesn't have a maximum amount.  This
-- requires using multiple requests, and using the marker param.
--
-- * Should 'AccountName' / 'ContainerID' / BlobID' check the naming rules?
--
-- * Should 'AccountKey' check that its the right size?  ATM if a
--   wrong-size key is used, an obtuse hmac error occurs.

-- Get Blob

-- | Fetches a blob from storage, specified by the 'ContainerID' and
-- 'BlobID'.
--
-- An optional inclusive byte range can be specified
-- (see http://msdn.microsoft.com/en-us/library/azure/ee691967.aspx).
--
-- This may throw 'BlobStorageException's or 'HttpException's.  In
-- particular, it will throw 'ContainerNotFound' or 'BlobNotFound' if
-- the container or blob id is wrong.
getBlob :: (MonadBaseControl IO m, MonadIO m, MonadResource m)
        => BlobStorage -> ContainerID -> BlobID -> Maybe (Int64, Int64)
        -> m (ResumableSource m ByteString)
getBlob bs (ContainerID cid) (BlobID bid) mrange =
    buildRequest bs "GET" [cid, bid] [] headers emptyRequestBody checkGotten >>=
    runRequest bs
  where
    checkGotten sig res consumeBody =
      if responseStatus res == Status 404 "The specified blob does not exist."
         then throwM BlobNotFound
         else checkErrorCode sig res consumeBody
    headers = case mrange of
        Nothing -> []
        --EKB TESTME
        Just (start, end) ->
            [ ("Range", encodeUtf8 $ Text.pack $ "bytes=" ++ show start ++ "-" ++ show end)
            ]

-- | Convenient wrapper over 'getBlob' to simply get a 'ByteString'
-- for a blob.
getBlobBytes :: BlobStorage -> ContainerID -> BlobID -> Maybe (Int64, Int64) -> IO ByteString
getBlobBytes bs cid bid mrange = runResourceT $ do
  source <- getBlob bs cid bid mrange
  toStrict <$> (source $$+- sinkLbs)

-- Delete Blob

-- | Marks the specified blob for deletion, such that the blob can
-- later be deleted during garbage collection.  This does not work on
-- blobs with snapshots (doing that requires using the
-- \"x-ms-delete-snapshots\" param).
deleteBlob :: BlobStorage -> ContainerID -> BlobID -> IO ()
deleteBlob bs (ContainerID cid) (BlobID bid) = runResourceT $ do
  req <- buildRequest bs "DELETE" [cid, bid] [] [] mempty checkErrorCode
  void $ runRequest bs req

-- Put Blob

-- | Updates a blob in storage, specified by the 'ContainerID' and
-- 'BlobID'.  The maximum blob size is 40gb.
--
-- NOTE: the maximum supported by azure is 200gb, but this function
-- would need to commit multiple sets of 10000 blobs.
--
-- This may throw 'BlobStorageException's or 'HttpException's.  If the
-- command is successful, but yields an unrecognized response, then
-- 'BlobNotCreated' is thrown.
putBlob :: (MonadIO m, MonadThrow m)
        => BlobStorage -> ContainerID -> BlobID
        -> Source m ByteString
        -> m ()
putBlob bs cid bid source = do
    -- Send the blocks, and then commit the blob.
    results <- source $= splitBytes maxBlockSize $= send 0 $$ CL.consume
    liftIO $ putBlockList bs cid bid results
  where
    send ix
      | ix >= maxUncommittedBlocks =
        throwM PutBlobRequestBodyTooLarge
      | otherwise = do
        mx <- await
        case mx of
          Nothing -> return ()
          Just x -> do
            let blockID = BlockID ix
            liftIO $ putBlock bs cid bid blockID x
            yield (blockID, BlockUncommitted)
            send (ix + 1)

-- | Convenient wrapper over 'putBlob' to simply put a 'ByteString'.
putBlobBytes :: BlobStorage -> ContainerID -> BlobID -> ByteString -> IO ()
putBlobBytes bs cid bid = putBlob bs cid bid . yield

-- | Like 'putBlob', except it uses only one request, and therefore is
-- limited to 64mb.
--
-- Note that 'RequestBodyStreamChunked' is not supported, as it does
-- not provide a way to get the total length.
_rawPutBlob :: BlobStorage -> ContainerID -> BlobID -> RequestBody -> IO ()
_rawPutBlob bs (ContainerID cid) (BlobID bid) body = runResourceT $ do
    req <- buildRequest bs "PUT" [cid, bid] [] headers body checkCreated
    void $ runRequest bs req
  where
    headers = [ (CI.mk "x-ms-blob-type", "BlockBlob") ]

-- | Convenient wrapper over 'putBlob' to simply put a 'ByteString' to
-- a blob.
_rawPutBlobBytes :: BlobStorage -> ContainerID -> BlobID -> ByteString -> IO ()
_rawPutBlobBytes bs cid bid = _rawPutBlob bs cid bid . RequestBodyBS

-- Put Block
-- (used by Put Blob)

data BlockLocation
  = BlockCommitted
  | BlockUncommitted
  | BlockLatest
  deriving (Eq, Read, Show)

type BlockList = [(BlockID, BlockLocation)]

-- Maximum block size = 4mb
maxBlockSize :: Integral a => a
maxBlockSize = 4 * 1024 * 1024

maxUncommittedBlocks :: Int
maxUncommittedBlocks = 10000

-- While 4 digits ids are large enough for id < maxUncommittedBlocks,
-- it's more future proof to have more digits, in order to allow for a
-- larger number of blocks in a blob.
blockIDLength :: Int
blockIDLength = 5

putBlock :: BlobStorage -> ContainerID -> BlobID -> BlockID -> RequestBody -> IO ()
putBlock bs (ContainerID cid) (BlobID bid) blockid body = runResourceT $ do
  l <- requestBodyLength body
  when (l > maxBlockSize) $ throwM (PutBlockRequestBodyTooLarge l)
  blockidText <- blockIDText blockid
  let query = [ ("comp", Just "block")
              , ("blockid", Just blockidText) ]
  req <- buildRequest bs "PUT" [cid, bid] query [] body checkCreated
  void $ runRequest bs req

putBlockList :: BlobStorage -> ContainerID -> BlobID -> BlockList -> IO ()
putBlockList bs (ContainerID cid) (BlobID bid) blocks = runResourceT $ do
  doc <- blockListDocument blocks
  let query = [ ("comp", Just "blocklist") ]
      body = RequestBodyLBS $ XML.renderLBS def doc
  req <- buildRequest bs "PUT" [cid, bid] query [] body checkCreated
  void $ runRequest bs req

blockListDocument :: (Functor m, MonadThrow m) => BlockList -> m Document
blockListDocument xs =
    (\root -> Document (Prologue [] Nothing []) root []) .
      Element "BlockList" mempty <$>
      forM xs (\(bid, loc) -> do
        c <- blockIDText bid
        return $ NodeElement $ Element (ename loc) mempty [NodeContent c])
  where
    ename BlockCommitted = "Committed"
    ename BlockUncommitted = "Uncommitted"
    ename BlockLatest = "Latest"

blockIDText :: MonadThrow m => BlockID -> m Text
blockIDText (BlockID x)
  | x < 0 = throwM $ BlockIDTooSmall x
  | x >= maxUncommittedBlocks = throwM $ BlockIDTooLarge x
  | otherwise = return $ encodeBase64Text $ pad blockIDLength "0" $ Text.pack $ show x

-- List blobs

-- | Convenient wrapper over 'listBlobsSource' to just get a list of
-- 'BlobID'.
listBlobs :: BlobStorage -> ContainerID -> BlobID -> Int -> IO [BlobID]
listBlobs bs cid prefix maxResults = runResourceT $ do
  source <- listBlobsSource bs cid prefix maxResults
  source $$+- CL.consume

-- | Fetches a stream of all of the 'BlobID's of the blobs in the specified
-- container that begin with the given prefix. This request is limited to the
-- specified number of results.  Fetching all of the results (via multiple
-- requests) requires wrapping more of the API.
listBlobsSource :: (MonadBaseControl IO m, MonadIO m, MonadResource m)
                => BlobStorage -> ContainerID -> BlobID -> Int -> m (ResumableSource m BlobID)
listBlobsSource bs cid prefix maxResults = do
  source <- fetchBlobList bs cid prefix maxResults
  return $ source $=+ XML.parseBytes XML.def $=+ extractBlobIDs

-- | This is just useful for debugging blob list requests (allows the
-- raw data to be fetched)
_listBlobsRaw :: BlobStorage -> ContainerID -> BlobID -> Int -> IO ByteString
_listBlobsRaw bs cid prefix maxResults = runResourceT $ do
  source <- fetchBlobList bs cid prefix maxResults
  toStrict <$> (source $$+- sinkLbs)

fetchBlobList :: (MonadThrow m, MonadResource m)
              => BlobStorage
              -> ContainerID
              -> BlobID
              -> Int
              -> m (ResumableSource m ByteString)
fetchBlobList bs (ContainerID cid) (BlobID prefix) maxResults = do
    req <- buildRequest bs "GET" [cid] query [] emptyRequestBody checkErrorCode
    runRequest bs req
  where
    query =
      [ ("restype", Just "container")
      , ("comp", Just "list")
      , ("maxresults", Just $ Text.pack $ show maxResults)
      , ("prefix", Just prefix)
      ]

extractBlobIDs :: MonadThrow m => Conduit XML.Event m BlobID
extractBlobIDs = lookForBlobTag
  where
    lookForBlobTag = do
      x <- await
      case x of
        Just (XML.EventBeginElement "Blob" _) -> extractBlobName
        Just _ -> lookForBlobTag
        Nothing -> return ()
    extractBlobName = do
      x <- await
      case x of
        Just (XML.EventBeginElement "Name" _) -> do
          yield =<< fmap BlobID XML.content
          lookForEndTag
        Just (XML.EventEndElement "Blob") -> lookForBlobTag
        Just _ -> extractBlobName
        Nothing -> return ()
    lookForEndTag = do
      x <- await
      case x of
        Just (XML.EventEndElement "Blob") -> lookForBlobTag
        Just _ -> lookForEndTag
        Nothing -> return ()
