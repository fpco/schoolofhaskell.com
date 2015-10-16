module Import.GroupDesc
    ( externalDesc
    , internalDesc
    ) where

import ClassyPrelude.Yesod
import Data.Text (breakOn)

externalDesc :: Text -> Text
externalDesc x = maybe x fst $ splitter x

internalDesc :: Text -> Text
internalDesc x = maybe x snd $ splitter x

splitter :: Text -> Maybe (Text, Text)
splitter x = do
    let (y, z) = breakOn breaker $ filter (/= '\r') x
    z' <- stripPrefix breaker z
    return (y, z')
  where
    breaker = "\n---\n"
