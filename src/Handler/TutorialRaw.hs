module Handler.TutorialRaw where

import Import

getTutorialRawR :: UserId -> BlobSHA -> Handler Text
getTutorialRawR uid blob = do
    eres <- getFrozenTutorialBlob uid blob
    case eres of
        Left _ -> notFound
        Right pt -> return $ unTutorialContent $ publishedTutorialContent pt
