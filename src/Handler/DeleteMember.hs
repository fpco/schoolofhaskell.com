module Handler.DeleteMember where

import Import

postDeleteMemberR :: TutorialName -> TutorialNames -> Handler Html
postDeleteMemberR tn tns = do
    Entity _ Profile {..} <- requireProfile

    let parent
            | null tns = UserR profileHandle
            | otherwise = UserTutorialR profileHandle tn $ allButLast tns

    mconfirm <- lookupPostParam confirm

    eres <- $runDB $ do
        eres <- followSlugPath profileUser tn tns
        (mid, Entity _ content, _) <- either (const notFound) return eres
        m <- get404 mid
        (ownerUsages, foreignUsages) <- findOtherUsages mid (tmemberContent m)

        let helper :: (PersistEntity value, PersistEntityBackend value ~ SqlBackend)
                   => Key value
                   -> (value -> Maybe UserId)
                   -> Text
                   -> YesodDB App (Either Html (Route App))
            helper id' getUser word =
                case mmsg of
                    Nothing -> do
                        deleteWhere [TmemberContent ==. tmemberContent m]
                        muid <- getUser <$> get404 id'
                        forM_ muid populateUserSummary
                        setMessage $ toHtml $ word ++ " deleted"
                        return $ Right parent
                    Just msgs -> lift $ fmap Left $ defaultLayout $ do
                        setTitle "Confirm deletion"
                        [whamlet|
                            <form method=post>
                                $forall msg <- msgs
                                    <p>#{msg}
                                <div>
                                    <button name=#{confirm} .confirm-button value=confirm>Confirm
                                    <a href=@{UserTutorialR profileHandle tn tns}>Cancel
                        |]
              where
                word' = toLower word
                mmsg =
                    case (null foreignUsages, mconfirm) of
                        (True, Nothing) -> Just ["Are you sure you wish to remove this " ++ word' ++ "?"]
                        (False, Nothing) -> Just
                            $ (word ++ " is linked from the following foreign locations. Are you sure you wish to remove it?")
                            : foreignUsages
                        (_, Just _) -> Nothing

        if null ownerUsages
            then do
                -- No other links exist from the current user.
                case content of
                    TcontentGroupSum gid -> do
                        -- Only delete groups if they are empty.
                        Entity hid _ <- getGroupHolder gid
                        children <- count [TmemberHolder ==. hid]
                        if children > 0
                            then do
                                lift $ setMessage "Cannot remove non-empty group. Please first remove all contents."
                                return $ Right $ UserTutorialR profileHandle tn tns
                            else do
                                case (null foreignUsages, mconfirm) of
                                    (False, Nothing) -> lift $ fmap Left $ defaultLayout $ do
                                        setTitle "Confirm deletion"
                                        [whamlet|
                                            <form method=post>
                                                <p>Other users link to this group. Are you sure you wish to delete it?
                                                <div>
                                                    <button .confirm-button name=#{confirm} value=confirm>Confirm
                                                    <a href=@{UserTutorialR profileHandle tn tns}>Cancel
                                        |]
                                    _ -> do
                                        deleteCascade gid
                                        setMessage "Group deleted"
                                        return $ Right parent
                    TcontentTutorialSum tid -> do
                        deleteWhere [RecentTutorialTutorial ==. tid]
                        helper tid (Just . tutorialAuthor) "Tutorial"
                    TcontentProjectSum _ -> fail "Cannot delete project from schoolofhaskell.com, instead use fpcomplete.com"
            else do
                -- Other links exists, we can delete this one
                delete mid
                setMessage "Content deleted"
                return $ Right parent
    case eres of
        Left html -> return html
        Right dest -> redirect dest
  where
    allButLast [] = []
    allButLast [_] = []
    allButLast (x:xs) = x : allButLast xs

    confirm = asText "confirm"
