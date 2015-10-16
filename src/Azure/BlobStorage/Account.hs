{-# LANGUAGE OverloadedStrings #-}

module Azure.BlobStorage.Account where

import           Azure.BlobStorage.Types
import           Control.Monad.Trans.Resource
import qualified Data.ByteString as BS
import           Data.Maybe (fromMaybe)
import           Network.HTTP.Conduit
import           Prelude

-- | This provides default settings for using the azure storage emulator:
--
-- <http://msdn.microsoft.com/en-us/library/azure/hh403989.aspx>
--
-- Note that if you want to use an HTTP manager with this,
-- you'll need to set it via the 'bsManager' field.
storageEmulator :: BlobStorage
storageEmulator =
    fromMaybe (error "Failed to parse azure storage emulator URL.") $
      blobStorageFromUrl
        "http://127.0.0.1:10000/devstoreaccount1"
        "devstoreaccount1"
        key
  where
    key = either (\_ -> error "Failed to parse azure storage emulator key.") id
        $ parseSharedKey
        $ BS.concat
        [ "Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6"
        , "IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw=="
        ]

-- | Uses components of the provided URL to initialize 'BlobStorage'
-- configuration.  For example:
--
-- > blobStorageFromUrl "https://test.blob.core.windows" "test" key
--
-- It also works for storage emulator URLs:
--
-- > blobStorageFromUrl
-- >   "http://127.0.0.1:10000/devstoreaccount1"
-- >   "devstoreaccount1"
-- >   key
--
-- Note that if you want to use an HTTP manager with this,
-- you'll need to set it via the 'bsManager' field.
blobStorageFromUrl :: MonadThrow m
                   => String
                   -> AccountName
                   -> SharedKey
                   -> m BlobStorage
blobStorageFromUrl url name key  = do
  req <- parseUrl url
  return $ BlobStorage
    { bsAccountName = name
    , bsAccountKey = key
    , bsHost = host req
    , bsPort = port req
    , bsSecure = secure req
    , bsPathPrefix = case path req of
        "/" -> ""
        p -> p
    , bsManager = Nothing
    }
