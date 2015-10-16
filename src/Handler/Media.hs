module Handler.Media where

import Import
import qualified FP.Store.Blob as Blob
import Blaze.ByteString.Builder (fromByteString)

getMediaR :: Blob.MediaUUIDType -> Handler ()
getMediaR sut@(Blob.MediaUUIDType _ mtype) = do
    let mime = Blob.mediaTypeMime mtype
    rsrc <- getFileSourceBlob $ Blob.Media sut
    (src, _) <- liftResourceT $ unwrapResumable rsrc
    sendResponse (mime, ContentSource $ mapOutput (Chunk . fromByteString) src)
