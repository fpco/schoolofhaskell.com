{-# LANGUAGE CPP #-}
module Handler.BuildVersion where

import Import
import Data.FileEmbed (embedOneFileOf)

buildVersion :: ByteString
buildVersion = $(embedOneFileOf ["config/build-version", "config/default-build-version"])

getBuildVersionR :: Handler RepPlain
getBuildVersionR = return $ RepPlain $ toContent buildVersion
