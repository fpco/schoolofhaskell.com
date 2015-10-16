{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Azure.BlobStorage.Types where

import           Azure.BlobStorage.Util
import           Control.Exception
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import           Data.Data (Typeable)
import qualified Data.Digest.Pure.SHA as SHA
import           Data.Int (Int64)
import           Data.String (IsString(..))
import           Data.Text (Text)
import           Network.HTTP.Conduit
import           Prelude

-- | This datatype stores the info necessary to make Azure blob
-- storage requests.
--
-- Note that for good performance, 'Just' values for 'bsManager' and
-- 'bsGen' should be provided.
data BlobStorage = BlobStorage
  { bsAccountName :: AccountName   -- ^ Name of the blob storage account.
  , bsAccountKey  :: SharedKey     -- ^ Key deserialized from a Base64 key.
  , bsHost        :: ByteString    -- ^ \"accountname.blob.core.windows.net\"
  , bsPort        :: Int           -- ^ Destination port.
  , bsSecure      :: Bool          -- ^ Use https? (You probably should!)
  , bsPathPrefix  :: ByteString
  -- ^ Prefix for the request path.  The main usecase for this is
  -- prepending the account name when using the development server.
  -- Otherwise, leave it empty.
  , bsManager     :: Maybe Manager -- ^ Optional HTTP manager.
  }

-- | ASCII identifier for the storage account.
--
-- From the documentation at
-- <http://msdn.microsoft.com/en-us/library/dd135715.aspx>:
--
-- A name for the storage account that is unique within Azure. Storage
-- account names must be between 3 and 24 characters in length and use
-- numbers and lower-case letters only.
--
-- This name is the DNS prefix name and can be used to access blobs,
-- queues, and tables in the storage account.
newtype AccountName = AccountName ByteString
  deriving (Eq, Show, Read, IsString)

-- | UTF8 identifier of the blob container.  These names must conform
-- to the following naming rules, taken from the documentation at
-- <http://msdn.microsoft.com/en-us/library/dd135715.aspx>:
--
-- 1. Container names must start with a letter or number, and can
-- contain only letters, numbers, and the dash (-) character.
--
-- 2. Every dash (-) character must be immediately preceded and
-- followed by a letter or number; consecutive dashes are not
-- permitted in container names.
--
-- 3. All letters in a container name must be lowercase.
--
-- 4. Container names must be from 3 through 63 characters long.
newtype ContainerID = ContainerID Text
  deriving (Eq, Show, Read, IsString)

-- | UTF8 identifier for a blob.  These names must conform to the
-- following naming rules, taken from the documentation at
-- <http://msdn.microsoft.com/en-us/library/dd135715.aspx>:
--
-- Note: Avoid blob names that end with a dot (.), a forward slash
-- (/), or a sequence or combination of the two.
--
-- A blob name can contain any combination of characters, … . A blob
-- name must be at least one character long and cannot be more than
-- 1,024 characters long. Blob names are case-sensitive.
newtype BlobID = BlobID Text
  deriving (Eq, Show, Read, IsString)

-- | The server considers BlockIDs to be base64 strings less than 64
-- bytes long.  Here, since the client gets to pick, we use a blob
-- index @ < 10000 @ (the maximum number of blocks in a blob)
--
-- Note that the API requires that all of the block-ids used by a blob
-- be the same length (upto 64 bytes).  In order to do this, we just
-- limit it to 9999, and pad the key sent to azure with leading 0s.
newtype BlockID = BlockID Int
  deriving (Eq, Show, Read)

-- | Represents the key shared for blob storage authentication.  The
-- constructor shouldn't be used, and isn't exported.
newtype SharedKey = SharedKey ByteString
  deriving (Eq, Show, Read)

-- | Parse a base-64 encoded storage account key.
parseSharedKey :: ByteString -> Either String SharedKey
parseSharedKey input = fmap SharedKey (Base64.decode input)

-- | Parse a base-64 encoded storage account key, using 'error' to
-- report base-64 parse exceptions.
parseSharedKeyOrFail :: ByteString -> SharedKey
parseSharedKeyOrFail = either error id . parseSharedKey

hmacSha256 :: SharedKey -> ByteString -> ByteString
hmacSha256 (SharedKey k) =
  toStrict .
  SHA.bytestringDigest . SHA.hmacSha256 (fromStrict k) .
  fromStrict

-- | These are some of the common exceptions that the blob storage
-- operations can encounter.  When possible, these are thrown instead
-- of 'StatusCodeException'.
data BlobStorageException
  = SignatureMismatch { ours, theirs :: Text }
  | AccountKeyWrong
  | BlobNotCreated
  | BlobNotFound
  | ContainerNotFound
  | ContainerAlreadyExists
  | InvalidErrorXml Text (Maybe HttpException)
  | RequestBodyStreamChunkedNotSupported
  | PutBlobRequestBodyTooLarge
  | PutBlockRequestBodyTooLarge Int64
  | BlockIDTooLarge Int
  | BlockIDTooSmall Int
  deriving (Show, Typeable)

instance Exception BlobStorageException
