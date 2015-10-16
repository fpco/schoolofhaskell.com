module Handler.AdminLoginAnalytics where

import Import
import Blaze.ByteString.Builder (fromByteString)
import qualified Data.Conduit.List as CL
import Data.CSV.Conduit (writeHeaders, fromCSV)

getAdminLoginAnalyticsR :: Handler TypedContent
getAdminLoginAnalyticsR =
    respondSourceDB typePlain
        $  selectSource [] []
        $= CL.map toRow
        $= (writeHeaders def >> fromCSV def)
        $= CL.concatMap (\bs -> [Chunk $ fromByteString bs, Flush])
  where
    toRow (Entity _ Login {..}) = asMap $ mapFromList
        [ ("user" :: Text, toPathPiece loginUser)
        , ("plugin", loginPlugin)
        , ("when", tshow loginWhen)
        ]
