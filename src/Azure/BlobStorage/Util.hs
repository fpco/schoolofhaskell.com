module Azure.BlobStorage.Util where

import qualified Blaze.ByteString.Builder as Builder
import           Control.Applicative
import           Control.Monad.Reader hiding (forM)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as Char
import           Data.Conduit
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Time (getCurrentTime, formatTime)
import           Network.HTTP.Conduit
import           Prelude
import           Safe (readMay)
import           System.Locale (defaultTimeLocale)

-- | Use the read instance to remove escaping.  If that fails, then
-- just return the original.
unescapeText :: Text -> Text
unescapeText input = fromMaybe input $ readMay $ "\"" ++ Text.unpack input ++ "\""

asciiLowercase :: ByteString -> ByteString
asciiLowercase = Char8.map Char.toLower

toStrict :: LBS.ByteString -> ByteString
toStrict = BS.concat . LBS.toChunks

fromStrict :: BS.ByteString -> LBS.ByteString
fromStrict = LBS.fromChunks . (: [])

encodeBase64Text :: Text -> Text
encodeBase64Text = Text.decodeUtf8 . Base64.encode . Text.encodeUtf8

strip :: ByteString -> ByteString
strip = snd . Char8.span Char.isSpace . fst . Char8.spanEnd Char.isSpace

pad :: Int -> Text -> Text -> Text
pad n c bs = Text.replicate (n - Text.length bs) c <> bs

rfc1123Time :: IO String
rfc1123Time =
  formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" <$> getCurrentTime

-- | Conduit which coalesces results of the specified byte-length.
-- The last result may be shorter than the specified length.
splitBytes :: Monad m => Int -> Conduit ByteString m RequestBody
splitBytes stride = go 0 mempty
  where
    go l b = do
      mx <- await
      case mx of
        Nothing ->
          when (l > 0) $ yield $ RequestBodyBuilder (fromIntegral l) b
        Just x -> do
          let l' = l + BS.length x
          if l' < stride
             then go l' (b <> Builder.fromByteString x)
             else do
               let (before, after) = BS.splitAt (stride - l) x
               yield $ RequestBodyBuilder (fromIntegral stride) (b <> Builder.fromByteString before)
               when (BS.length after > 0) $ leftover after
               go 0 mempty
