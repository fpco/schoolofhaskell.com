module Import.Files where

import ClassyPrelude.Yesod hiding (fileName)
import Foundation
import qualified FP.Store.Blob as Blob

--EKB TODO Eliminate this by giving FP.Store.Blob a 'HasBlobStore' typeclass like
-- https://github.com/fpco/stackage-server/blob/master/Data/BlobStore.hs

getFileSourceBlob
    :: Blob.BlobName
    -> Handler (ResumableSource (ResourceT IO) ByteString)
getFileSourceBlob blobuuid = do
    y <- getYesod
    liftResourceT $ Blob.get (appBlobStore y) blobuuid

putFileSourceBlob
    :: Blob.BlobName
    -> Source (ResourceT IO) ByteString
    -> Int64
    -> Handler ()
putFileSourceBlob file src size = do
    y <- getYesod
    liftResourceT $ Blob.put (appBlobStore y) file src size
