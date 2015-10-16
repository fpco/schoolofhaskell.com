{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Arrange where

import           Control.Monad (zipWithM)
import           Data.Aeson (decode)
import qualified Data.Conduit.List as CL
import           Import

getArrangeR :: TutorialNames -> Handler Html
getArrangeR tns = do
    uid <- requireAuthId
    Entity _ profile <- $runDB $ getBy404 $ UniqueProfile uid

    -- Gets all of the subchildren of the given holder, in the format expected
    -- by the arrangement UI.  Groups are only traversed if they're owned by the
    -- user otherwise they don't have permission to modify priority.
    --
    -- Implementation patterned after 'findUnderGroup'.
    seenRef <- newIORef (asSet mempty)
    let rec :: HolderId -> YesodDB App (HtmlUrl url)
        rec hid = do
            seen <- readIORef seenRef
            if hid `member` seen
                then return mempty
                else do
                    modifyIORef seenRef $ insertSet hid
                    lis <- selectSource [TmemberHolder ==. hid]
                                        [Asc TmemberPriority]
                        $$ CL.mapM go
                        =$ sinkList
                    return [hamlet|
                        $forall li <- lis
                            ^{li}
                      |]
        go :: Entity Tmember -> YesodDB App (HtmlUrl url)
        go (Entity mid Tmember {..}) = do
            let k = toPathPiece mid
            mc <- get tmemberContent
            case mc of
                Nothing -> return mempty
                Just (TcontentGroupSum gid) -> do
                    g <- get404 gid
                    let isOwned = tgroupAuthor g == uid
                    mnested <-
                        if isOwned
                            then do
                                Entity hid _ <- getGroupHolder gid
                                Just <$> rec hid
                            else return Nothing
                    return [hamlet|
                        <li .arrange-group data-key=k#{k}>
                            <span>#{tgroupTitle g}
                            $maybe nested <- mnested
                                <ol>
                                    ^{nested}
                      |]
                Just (TcontentTutorialSum tid) -> do
                    t <- get404 tid
                    return [hamlet|
                        <li .arrange-tutorial data-key=k#{k}>
                            <span>#{tutorialTitle t}
                      |]
                Just (TcontentProjectSum _) ->
                    return [hamlet|
                        <li .arrange-project data-key=k#{k}>
                            <span>FPHC project
                      |]

    content <- $runDB $ rec . entityKey =<< getTopHolder uid

    let returnRoute =
            case tns of
                (tn:tns') -> UserTutorialR (profileHandle profile) tn tns'
                [] -> UserR $ profileHandle profile

    defaultLayout $ do
        setTitle $ "Arrange - School of Haskell"
        $(combineScripts 'StaticR
            [ js_jquery_sortable_js
            ])
        $(widgetFile "arrange")

postArrangeR :: TutorialNames -> Handler ()
postArrangeR _ = do
    Entity _ profile <- requireProfile
    let uid = profileUser profile
    mout <- lookupPostParam "arrangement"
    $runDB $
        case mout >>= decode . encodeUtf8 . repack of
            Nothing -> invalidArgs ["Error while deserializing arrangement"]
            Just xs -> do
                Entity thid _ <- getBy404 $ UniqueHolderTopLevel uid
                mms <- zipWithM (getMove thid uid) [0..] xs
                let ms = catMaybes mms
                mapM_ (delete . fst) ms
                mapM_ (doInsert . snd) ms
  where
    doInsert :: Tmember -> YesodDB App ()
    doInsert m = do
        mk <- insertUnique m
        when (isNothing mk) $ do
            let title = unTutorialName $ tmemberSlug m
            slug <- getUniqueSlug (tmemberHolder m) (Title title)
            _ <- insert $ m { tmemberSlug = slug }
            return ()
    getMove :: Key Holder
            -> Key User
            -> Int
            -> (Maybe Text, Text)
            -> YesodDB App (Maybe (Key Tmember, Tmember))
    getMove thid uid priority (mpk, k) = do
        Entity mid m <- getMember uid k
        m' <-
            case mpk of
                Nothing ->
                    return $ m
                        { tmemberPriority = priority
                        , tmemberHolder = thid
                        }
                Just pk -> do
                    Entity _ pm <- getMember uid pk
                    content <- get404 $ tmemberContent pm
                    case content of
                        TcontentGroupSum gid -> do
                            Entity hid _ <- getGroupHolder gid
                            return $ m
                                { tmemberPriority = priority
                                , tmemberHolder = hid
                                }
                        _ -> invalidArgs ["Invalid group key: " ++ pk]
        return $ if m == m' then Nothing else Just (mid, m')
    getMember :: Key User -> Text -> YesodDB App (Entity Tmember)
    getMember uid kattr =
        case fromPathPiece kattr of
            Nothing -> invalidArgs ["Error while deserializing key: " ++ kattr]
            Just k' -> do
                let mid = TmemberKey k' :: Key Tmember
                mm <- get mid
                case mm of
                    Nothing -> invalidArgs ["Invalid key: " ++ kattr]
                    Just m -> do
                        -- Check that the user has permission to modify this
                        -- membership relationship.
                        holder <- get404 $ tmemberHolder m
                        case holder of
                            HolderTopLevelSum tuid ->
                                when (tuid /= uid) $
                                    invalidArgs ["Invalid key: " ++ kattr]
                            HolderGroupSum gid -> do
                                g <- get404 gid
                                when (tgroupAuthor g /= uid) $
                                    invalidArgs ["Invalid key: " ++ kattr]
                        return $ Entity mid m
