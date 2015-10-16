module Handler.LikeContent where

import Import

getLikeContentR :: TcontentId -> Handler Value
getLikeContentR tcid = do
    muid <- maybeAuthId
    $runDB $ do
        myLike <-
            case muid of
                Nothing -> return "not-logged-in"
                Just uid -> fmap (maybe "no-like" $ const "like") $ getBy $ UniqueContentLike tcid uid
        cnt <- count [ContentLikeContent ==. tcid]
        return $ object
            [ "myLike" .= asText myLike
            , "likes" .= cnt
            ]

postLikeContentR :: TcontentId -> Handler Value
postLikeContentR tcid = do
    muid <- maybeAuthId
    res <- case muid of
        Nothing -> return $ Left "Not logged in"
        Just uid -> do
            unlike <- isJust <$> lookupPostParam "unlike"
            $runDB $ do
                msg <- if unlike
                    then do
                        deleteBy $ UniqueContentLike tcid uid
                        return "You have unliked this content"
                    else do
                        now <- liftIO getCurrentTime
                        void $ insertBy $ ContentLike tcid uid now
                        return "You have liked this content"
                cnt <- count [ContentLikeContent ==. tcid]
                return $ Right (cnt, msg)
    return $ object $ case res :: Either Text (Int, Text) of
        Left e ->
            [ "status" .= (asText "failure")
            , "message" .= e
            ]
        Right (likes, t) ->
            [ "status" .= (asText "success")
            , "message" .= t
            , "likes" .= likes
            ]
