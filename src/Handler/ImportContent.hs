module Handler.ImportContent where

import Import
import qualified Network.URI

form :: Entity Holder -> Form (Text, (TcontentId, HolderId, TmemberId))
form (Entity holderid holder) =
    renderDivs $ areq (checkMMap parse render urlField) "Content URL" Nothing
  where
    render = fst
    parse url =
        case Network.URI.parseURI $ unpack url of
            Nothing -> return $ Left $ asText "Invalid URL"
            Just Network.URI.URI { Network.URI.uriPath = up } -> do
                let mres =
                        case parseRoute (decodePathSegments $ encodeUtf8 $ pack up, []) of
                            Just (SchoolTutorialR x xs) -> Just (NormalizedHandle "school", x, xs)
                            Just (UserTutorialR user' x xs) ->
                                case normalizeHandle user' of
                                    Left _ -> Nothing
                                    Right user -> Just (user, x, xs)
                            _ -> Nothing
                case mres of
                    Nothing -> return $ Left "Did not understand URL"
                    Just (uh, tn, tns) -> $runDB $ do
                        mprofile <- getBy $ UniqueNormalizedHandle uh
                        case mprofile of
                            Nothing -> return $ Left "Invalid user"
                            Just (Entity _ profile) -> do
                                eres <- followSlugPath (profileUser profile) tn tns
                                case eres of
                                    Left e -> return $ Left $ tshow e
                                    Right (mid, Entity cid (TcontentProjectSum _), _) -> return $ Right (url, (cid, holderid, mid))
                                    Right (mid, Entity cid (TcontentTutorialSum _), _) -> return $ Right (url, (cid, holderid, mid))
                                    -- If its a group, then check that it isn't any of our ancestors.
                                    Right (mid, Entity cid (TcontentGroupSum gid), _) -> do
                                        case holder of
                                            HolderTopLevelSum _ -> return $ Right (url, (cid, holderid, mid))
                                            HolderGroupSum hgid
                                                | hgid /= gid -> do
                                                    mContentHolder <- getBy $ UniqueContentGroup hgid
                                                    case mContentHolder of
                                                        Nothing -> return $ Right (url, (cid, holderid, mid))
                                                        Just (Entity hcid _) -> do
                                                            foundLoop <- findGroupAncestor hcid
                                                                (\k -> return $ if k == gid then Just () else Nothing)
                                                            case foundLoop of
                                                                Nothing -> return $ Right (url, (cid, holderid, mid))
                                                                _ -> return $ Left "Cannot construct loop"
                                                | otherwise -> return $ Left "Cannot construct loop"


getImportContentR :: TutorialNames -> Handler Html
getImportContentR tns = do
    Entity _ Profile {..} <- requireProfile
    unless (profileHandle == UserHandle "school") notFound
    holder <- $runDB $
        case tns of
            [] -> getTopHolder profileUser
            tn:tns' -> do
                eres <- followSlugPath profileUser tn tns'
                case eres of
                    Right (_, Entity _ (TcontentGroupSum gid), _) -> getBy404 $ UniqueHolderGroup gid
                    _ -> notFound
    ((res, widget), enctype) <- runFormPost $ form holder
    let backURL =
            case tns of
                [] -> UserR profileHandle
                tn:tns' -> UserTutorialR profileHandle tn tns'
    case res of
        FormSuccess (_, (a, b, c)) -> do
            $runDB $ addLink a b c
            setMessage "Link added"
            redirect backURL
        _ -> defaultLayout $ do
            setTitle "Add link"
            [whamlet|
                <form method=post enctype=#{enctype}>
                    ^{widget}
                    <div>
                        <button>Add link
                        <a href=@{backURL}>Cancel
            |]

postImportContentR :: TutorialNames -> Handler Html
postImportContentR = getImportContentR

addLink :: TcontentId -> HolderId -> TmemberId -> YesodDB App ()
addLink cid hid mid = do
    origMember <- get404 mid
    updateWhere [TmemberHolder ==. hid] [TmemberPriority +=. 1]
    void $ addMember cid hid (tmemberSlug origMember) (tmemberSlugUserGen origMember)

-- | Add some content to a holder, ensuring we have a unique slug based on
-- the given base slug.
addMember :: TcontentId -> HolderId -> TutorialName -> Bool -> YesodDB App TutorialName
addMember cid hid baseSlug@(TutorialName baseSlug') userGen =
    go Nothing
  where
    go msuffix = do
        eres <- insertBy Tmember
            { tmemberContent = cid
            , tmemberHolder = hid
            , tmemberSlug = slug
            , tmemberSlugUserGen = userGen'
            , tmemberPriority = 0
            }
        case eres of
            Left _ -> go msuffix'
            Right _ -> return slug
      where
        (slug, userGen', msuffix') =
            case msuffix of
                Nothing -> (baseSlug, userGen, Just (1 :: Int))
                Just i -> (TutorialName $ baseSlug' ++ tshow i, False, Just $ succ i)
