module Handler.MediaUpload where

import Import
import qualified FP.Store.Blob as Blob
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import System.IO (openTempFile)
import System.Directory (removeFile)
import Control.Monad.Trans.Resource (allocate)
import Prelude (Show (..))

getMediaUploadR :: Handler Html
getMediaUploadR = defaultLayout $ do
    setTitle "Upload Media file"
    [whamlet|
        <form method=post enctype=#{Multipart}>
            <input type=file name=file>
            <button>Upload file
    |]

postMediaUploadR :: Handler Value
postMediaUploadR = do
    files <- lookupFiles "file"
    eres <-
        case files of
            [] -> return $ Left "No file provided"
            [file] -> do
                case lookup (encodeUtf8 $ fileContentType file) Blob.mtypeMimeMap of
                    Nothing -> return $ Left $ "Unsupported mime type: " ++ fileContentType file
                    Just mtype -> do
                        uuid <- newMediaBlobUUID
                        let sut = Blob.MediaUUIDType uuid mtype
                        mencode <- lookupGetParam "encode"
                        let src =
                                if mencode == Just "base64"
                                    then fileSource file $= decodeBase64
                                    else fileSource file
                        (_, fp) <- allocate
                            (mask_ $ do
                                (fp, h) <- openTempFile "/tmp/" "upload.bin"
                                hClose h
                                return fp)
                            removeFile
                        len <- fmap fromIntegral $ liftResourceT $ src $$ CB.conduitFile fp =$ CL.fold (\l bs -> length bs + l) 0
                        void $ catch (putFileSourceBlob (Blob.Media sut) src len) $
                            throwIO . MediaBlobUploadException
                        return $ Right sut
            _ -> return $ Left "Too many files provided"
    render <- getUrlRender
    return $
        case eres of
            Left e -> object ["result" .= asText "failure", "message" .= e]
            Right sut -> object ["result" .= asText "success", "url" .= render (MediaR sut)]

data MediaBlobUploadException = MediaBlobUploadException SomeException
    deriving Typeable
instance Show MediaBlobUploadException where
    show (MediaBlobUploadException e) = "Could not upload media file to blob storage. Exception was: " ++ Prelude.show e
instance Exception MediaBlobUploadException

decodeBase64 :: Monad m => Conduit ByteString m ByteString
decodeBase64 = do
    CB.dropWhile (/= (toEnum $ fromEnum ','))
    CB.drop 1
    decodeBase64C
