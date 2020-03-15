{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Import.Tutorial where

import           ClassyPrelude.Yesod hiding (div, runDB)
import           Control.Monad.Logger
import           Control.Monad.Morph
import           Control.Monad.State.Strict (runState, modify)
import           Control.Monad.Trans.Control
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Base16 as B16
import           Data.Conduit.List (mapMaybeM, concatMapM)
import           Data.Text (strip)
import           Data.Text.Read (decimal)
import           Foundation
import           Import.GroupDesc
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html.Renderer.Utf8
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import           Text.HTML.DOM (parseLBS)
import           Text.HTML.SanitizeXSS (filterTags, safeTags, balanceTags)
import           Text.HTML.TagSoup (Tag (TagOpen, TagClose))
import           Text.Markdown
import           Text.XML (Node (..), Document (..), Element (..), Name (..))

class MonadLoggerLift m where
    liftLoggingT :: LoggingT IO a -> m a

transLoggingT :: (m a -> n b) -> LoggingT m a -> LoggingT n b
transLoggingT f (LoggingT mx) = LoggingT (f . mx)

instance MFunctor LoggingT where
    hoist f (LoggingT mx) = LoggingT (f . mx)
instance MMonad LoggingT where
    embed f m = LoggingT (\i -> runLoggingT (f (runLoggingT m i)) i)

transMonadLoggerLog
    :: (MonadBaseControl IO m, ToLogStr msg)
    => (Loc -> LogSource -> LogLevel -> msg -> m ())
    -> m (Loc -> LogSource -> LogLevel -> msg -> IO ())
transMonadLoggerLog f =
    liftBaseWith $ \run -> return $ \a b c d -> void $ run $ f a b c d

instance (MonadBaseControl IO m, MonadIO m)
         => MonadLoggerLift (HandlerT a m) where
    liftLoggingT (LoggingT m) = do
        log' <- transMonadLoggerLog monadLoggerLog
        liftIO $ m log'

instance (MonadBaseControl IO m, MonadIO m, MonadLogger m)
         => MonadLoggerLift (ReaderT a m) where
    liftLoggingT (LoggingT m) = do
        log' <- transMonadLoggerLog monadLoggerLog
        liftIO $ m log'

putFrozenTutorial :: MonadIO m => TutorialId -> Tutorial -> SqlPersistT m ()
putFrozenTutorial tid Tutorial {..} = do
    deleteBy $ UniquePublishedTutorial tid
    now <- liftIO getCurrentTime
    insert_ PublishedTutorial
        { publishedTutorialTutorial = tid
        , publishedTutorialAuthor = tutorialAuthor
        , publishedTutorialSha = BlobSHA $ decodeUtf8 $ B16.encode
                               $ SHA1.hash $ encodeUtf8 $ unTutorialContent tutorialContent
        , publishedTutorialTitle = tutorialTitle
        , publishedTutorialDesc = tutorialDesc
        , publishedTutorialContent = tutorialContent
        , publishedTutorialEnvironment = tutorialEnvironment
        , publishedTutorialTimestamp = now
        }

-- tutorialPath :: TutorialId -> TreeFilePath
-- tutorialPath tid = B8.pack . F.encodeString . fpFromText $
--     concat ["tutorials/tutorial-", toPathPiece tid, ".md"]

getFrozenTutorialBlob :: UserId
                      -> BlobSHA
                      -> Handler (Either TutorialException PublishedTutorial)
getFrozenTutorialBlob uid blob = do
    mres <- $runDB $ selectFirst
        [ PublishedTutorialAuthor ==. uid
        , PublishedTutorialSha ==. blob
        ]
        []
    return $ case mres of
        Nothing -> Left $ BlobNotFound blob
        Just (Entity _ pt) -> Right pt

data TutorialException = BlobNotFound BlobSHA
    deriving (Show, Typeable)
instance Exception TutorialException

data GetNamesHolderException = HolderDoesNotExist | NameNotAGroup
    deriving Typeable
instance Show GetNamesHolderException where
    show HolderDoesNotExist = "The given group does not exist"
    show NameNotAGroup = "The given path is not a group"
instance Exception GetNamesHolderException

getNamesHolder :: Bool -- ^ auto-create?
               -> UserId
               -> [TutorialName]
               -> YesodDB App (Either GetNamesHolderException (Entity Holder))
getNamesHolder autoCreate uid names0 = do
    top <- getTopHolder uid
    loop top names0
  where
    loop h [] = return $ Right h
    loop (Entity hid _) (name:names) = do
        mmember <- getBy $ UniqueMember hid name
        case mmember of
            Nothing
                | autoCreate -> do
                    now <- liftIO getCurrentTime
                    gid <- insert Tgroup
                        { tgroupAuthor = uid
                        , tgroupTitle = Title $ unTutorialName name
                        , tgroupDesc = Textarea ""
                        , tgroupUpdated = now
                        }
                    let holder = HolderGroupSum gid
                    hid' <- insert holder
                    cid <- insert $ TcontentGroupSum gid
                    mlastMember <- selectFirst [TmemberHolder ==. hid] [Desc TmemberPriority]
                    let priority =
                            case mlastMember of
                                Nothing -> 1
                                Just (Entity _ m) -> tmemberPriority m + 1
                    insert_ Tmember
                        { tmemberContent = cid
                        , tmemberHolder = hid
                        , tmemberSlug = name
                        , tmemberSlugUserGen = True -- to avoid automatic renaming
                        , tmemberPriority = priority
                        }
                    loop (Entity hid' holder) names
                | otherwise -> return $ Left HolderDoesNotExist
            Just (Entity _ Tmember {..}) -> do
                mtc <- get tmemberContent
                case mtc of
                    Just (TcontentGroupSum gid) -> do
                        mh <- getBy $ UniqueHolderGroup gid
                        h <-
                            case mh of
                                Just h -> return h
                                Nothing -> do
                                    let holder = HolderGroupSum gid
                                    hid' <- insert holder
                                    return $ Entity hid' holder
                        loop h names
                    _ -> return $ Left NameNotAGroup

getGroupHolder :: TgroupId -> YesodDB App (Entity Holder)
getGroupHolder gid = do
    mholder <- getBy $ UniqueHolderGroup gid
    case mholder of
        Nothing -> do
            let h = HolderGroupSum gid
            hid <- insert h
            return $ Entity hid h
        Just h -> return h

-- | Get the contents of the given user's top level holder.
getTopContents :: (TutorialName -> url)
               -> UserId
               -> YesodDB App [HolderContent (Maybe UTCTime) url]
getTopContents toURL uid =
    getTopHolder uid >>= getHolderContents toURL

getTopHolder :: MonadIO m => UserId -> SqlPersistT m (Entity Holder)
getTopHolder uid = do
    mholder <- getBy $ UniqueHolderTopLevel uid
    case mholder of
        Nothing -> do
            let h = HolderTopLevelSum uid
            hid <- insert h
            return $ Entity hid h
        Just h -> return h

-- | Get a unique slug based on the given title for the given group.
getUniqueSlug :: MonadIO m
              => HolderId
              -> Title
              -> SqlPersistT m TutorialName
getUniqueSlug holder title = do
    loop Nothing
  where
    loop i = do
        mt <- getBy $ UniqueMember holder slug
        case mt of
            Nothing -> return slug
            Just _ -> next
      where
        slug = TutorialName slug'
        (next, slug') =
            case i :: Maybe Int of
                Nothing -> (loop $ Just 1, baseSlug)
                Just j -> (if j > 50
                            then error "Could not find unique slug"
                            else loop $ Just $ j + 1
                          , baseSlug ++ "-" ++ tshow j
                          )
    baseSlug = titleToSlug title

data TutContent url
    = TCGroup UserId Title Textarea [HolderContent (Maybe UTCTime) url]
    | TCTutorial UserId PublishedTutorial !TutPrev !TutNext !(Entity Tutorial)

type TutPrev = Maybe TutPrevNext
type TutNext = Maybe TutPrevNext
data TutPrevNext = TutPrevNext
    { tpnSlug :: !TutorialName
    , tpnTitle :: !Text
    }

followSlugPath :: UserId
               -> TutorialName
               -> [TutorialName]
               -> YesodDB App (Either FollowSlugPathException (TmemberId, Entity Tcontent, [(TutorialName, TgroupId)]))
followSlugPath uid tn0 tns0 = do
    holder0 <- getTopHolder uid
    loop id (asSet mempty) holder0 tn0 tns0
  where
    loop _ usedHolders (Entity holderid _holder) _ _ | holderid `member` usedHolders = return $ Left GroupCycle
    loop frontBCS usedHolders (Entity holderid _holder) tn tns = do
        mmember <- getBy $ UniqueMember holderid tn
        case mmember of
            Nothing -> return $ Left SlugPathNotFound
            Just (Entity mid Tmember {..}) -> do
                content <- get404 tmemberContent
                case (content, tns) of
                    (TcontentGroupSum gid, _) -> do
                        holder <- getGroupHolder gid
                        case tns of
                            [] -> return $ Right (mid, Entity tmemberContent content, frontBCS [])
                            tn':tns' -> loop
                                (frontBCS . ((tn, gid):))
                                (insertSet holderid usedHolders)
                                holder
                                tn'
                                tns'
                    (_, []) -> return $ Right (mid, Entity tmemberContent content, frontBCS [])
                    (_, _) -> return $ Left SlugPathNotFound

data FollowSlugPathException = GroupCycle
                             | SlugPathNotFound
    deriving (Show, Typeable)
instance Exception FollowSlugPathException

-- | Some legacy logic: checks both the PublishedTutorial table and Git.
getPublishedTutorial :: MonadIO m
                     => Entity Tutorial
                     -> ReaderT SqlBackend m (Maybe PublishedTutorial)
getPublishedTutorial (Entity tid Tutorial {..}) = do
    mpt <- getBy $ UniquePublishedTutorial tid
    case mpt of
        Nothing -> return Nothing
        Just (Entity _ pt) -> return $ Just pt

getTutPath :: UserId
           -> (TutorialName -> [TutorialName] -> url)
           -> TutorialName
           -> [TutorialName]
           -> YesodDB App (Maybe ([(url, Text)], TutContent url, TcontentId))
getTutPath uid toURL tn0 tns0 = do
    eres <- followSlugPath uid tn0 tns0
    case eres of
        Left _ -> return Nothing
        Right (tmid, Entity cid content, bcs) -> do
            let finalURL = toURL tn0 tns0
            mc <- fixContent tmid content
            case mc of
                Nothing -> return Nothing
                Just (content', title) -> do
                    bcs' <- fixBCs id id bcs
                    return $ Just (bcs' [(finalURL, unTitle title)], content', cid)
  where
    urlHelper to t = to tn0 $ tns0 ++ [t]
    toURL' = urlHelper toURL

    fixBCs _ front [] = return front
    fixBCs usedNames front ((tn, gid):rest) = do
        title <- unTitle . tgroupTitle <$> get404 gid
        fixBCs usedNames' (front . ((url, title):)) rest
      where
        usedNames' = usedNames . (tn:)
        url =
            case usedNames' [] of
                [] -> assert False $ toURL tn [] -- should never happen
                x:xs -> toURL x xs

    fixContent _ (TcontentGroupSum gid) = do
        Tgroup {..} <- get404 gid
        holder <- getGroupHolder gid
        contents <- getHolderContents toURL' holder
        return $ Just (TCGroup tgroupAuthor tgroupTitle tgroupDesc contents, tgroupTitle)
    fixContent tmid (TcontentTutorialSum tid) = do
        tm <- get404 tmid
        tut@Tutorial {..} <- get404 tid
        mft <- do
            mpt <- getPublishedTutorial (Entity tid tut)
            lift $ case mpt of
                Nothing -> notFound
                Just pt -> return pt
        let getPN ascDesc comp = selectSource
                [ TmemberHolder ==. tmemberHolder tm
                , TmemberPriority `comp` tmemberPriority tm
                ]
                [ ascDesc TmemberPriority
                ] $$ fixContentLoop

        mprev <- getPN Desc (<.)
        mnext <- getPN Asc (>.)
        return $ Just (TCTutorial tutorialAuthor mft mprev mnext (Entity tid tut), publishedTutorialTitle mft)
    fixContent  _ (TcontentProjectSum _) = return Nothing

    fixContentLoop :: Sink (Entity Tmember) (YesodDB App) (Maybe TutPrevNext)
    fixContentLoop = await >>= maybe (return Nothing) loop
        where
            loop :: Entity Tmember
                 -> Sink (Entity Tmember) (YesodDB App) (Maybe TutPrevNext)
            loop (Entity _ Tmember {..}) = do
                mtc <- lift $ get tmemberContent
                case mtc of
                    Just (TcontentTutorialSum tidPN) -> do
                        tut@Tutorial {..} <- lift $ get404 tidPN
                        mpt <- lift $ getPublishedTutorial $ Entity tidPN tut
                        case mpt of
                            Nothing -> fixContentLoop
                            Just PublishedTutorial {..} -> return $ Just TutPrevNext
                                { tpnSlug = tmemberSlug
                                , tpnTitle = unTitle publishedTutorialTitle
                                }
                    _ -> fixContentLoop

data HolderContent when url = HolderContent
    { hcURL              :: !url
    , hcTitle            :: !Title
    , hcDesc             :: !Textarea
    , hcWhen             :: !when
    , hcUser             :: !User
    , hcProfile          :: !Profile
    , hcType             :: !MemberType
    , hcSlug             :: !(Maybe TutorialName)
    , hcPublic           :: !Bool
    , hcForeignDeleteURL :: !(Maybe url)
    }

data MemberType = MTTutorial
                | MTGroup
  deriving Eq

getHolderContents :: (TutorialName -> url)
                  -> Entity Holder
                  -> YesodDB App [HolderContent (Maybe UTCTime) url]
getHolderContents toURL (Entity hid _) =
    selectSource [TmemberHolder ==. hid] [Asc TmemberPriority]
        $$ mapMaybeM (toHolderContent toURL)
        =$ sinkList

toHolderContent :: (TutorialName -> url)
                -> Entity Tmember
                -> YesodDB App (Maybe (HolderContent (Maybe UTCTime) url))
toHolderContent toURL (Entity _ Tmember {..}) = handleAny
  (\e -> $logWarn (tshow e) >> return Nothing) $ do
    content <- get404 tmemberContent
    mtuple <-
        case content of
            TcontentTutorialSum tid -> do
                tut@Tutorial {..} <- get404 tid
                mpt <- getPublishedTutorial $ Entity tid tut
                case mpt of
                    Nothing -> return Nothing
                    Just pt -> return $ Just
                        ( True
                        , tutorialVersionTitle' pt
                        , publishedTutorialDesc pt
                        , Just $ publishedTutorialTimestamp pt
                        , tutorialAuthor
                        , MTTutorial
                        )
            TcontentGroupSum gid -> do
                visible <- isGroupVisible gid
                if visible
                    then do
                        Tgroup {..} <- get404 gid
                        return $ Just (True, tgroupTitle, Textarea $ externalDesc $ unTextarea tgroupDesc, Nothing, tgroupAuthor, MTGroup)
                    else return Nothing
            TcontentProjectSum _ -> return Nothing

    case mtuple of
        Just (isPublic, title, desc, when', uid, memberType) -> do
            -- This should be impossible, but just in case...
            if not isPublic
                then do
                    $logError $ "toHolderContent attempted to publicize something private: " ++ unTitle title
                    return Nothing
                else do
                    user <- get404 uid
                    mprofile <- getBy $ UniqueProfile uid
                    case mprofile of
                        Nothing -> return Nothing
                        Just (Entity _ profile) -> return $ Just HolderContent
                            { hcURL = toURL tmemberSlug
                            , hcTitle = title
                            , hcDesc = desc
                            , hcWhen = when'
                            , hcUser = user
                            , hcProfile = profile
                            , hcType = memberType
                            , hcSlug = Just tmemberSlug
                            , hcPublic = isPublic
                            , hcForeignDeleteURL = Nothing
                            }
        Nothing -> return Nothing

-- | Determine if the group contains any public tutorials.
isGroupVisible :: TgroupId -> YesodDB App Bool
isGroupVisible gid = fromMaybe False <$> findUnderGroup gid memberVisible
  where
    memberVisible tid = do
            t <- get404 tid
            msha <- getPublishedTutorial $ Entity tid t
            return (const True <$> msha)

-- | Determine if the group contains any public tutorials, and checks if any
--   authored by the specified author are dirty.
isGroupVisibleOrDirty :: UserId -> TgroupId -> YesodDB App (Bool, Bool)
isGroupVisibleOrDirty uid gid = do
    visibleRef <- newIORef False
    let memberVisibleOrDirty tid = do
            t <- get404 tid
            msha <- getPublishedTutorial $ Entity tid t
            case (tutorialIsDirty t, msha, tutorialAuthor t == uid) of
                (True, Just _, True) -> return $ Just ()
                (_, Just _, _) -> writeIORef visibleRef True >> return Nothing
                _ -> return Nothing
    result <- findUnderGroup gid memberVisibleOrDirty
    case result of
        Just _ -> return (True, True)
        Nothing -> do
            visible <- readIORef visibleRef
            return (visible, False)

--TODO: both of these functions are brittle - they'll cause 404s for
--the user's entire dashboard / public view if anything goes wrong
--with the tree structure.

-- | Recursively searches a group for a 'Tutorial' that returns a non-Nothing
--   value for the provided function.  This function is in the 'YesodDB' monad
--   so that state can be used / more requests can be made.
findUnderGroup :: TgroupId -> (TutorialId -> YesodDB App (Maybe a)) -> YesodDB App (Maybe a)
findUnderGroup tgid f = do
    seenRef <- newIORef (asSet mempty)
    let rec gid = do
            seen <- readIORef seenRef
            if gid `member` seen
                then return Nothing
                else do
                    mh <- getBy $ UniqueHolderGroup gid
                    case mh of
                        Nothing -> return Nothing
                        Just (Entity hid _) ->
                            selectSource [TmemberHolder ==. hid] [] $$ loop
          where
            loop = await >>= maybe (return Nothing) go

            go (Entity _ Tmember {..}) = do
                c <- lift $ get404 tmemberContent
                result <-
                    case c of
                        TcontentProjectSum _ -> return Nothing
                        TcontentTutorialSum tid -> lift $ f tid
                        TcontentGroupSum gid' -> do
                            modifyIORef seenRef $ insertSet gid
                            lift $ rec gid'
                case result of
                    Just x -> return (Just x)
                    Nothing -> loop
    rec tgid

-- | Recursively searches above the group for an ancestor 'Group' that returns a
--   non-Nothing value for the provided function.  This function is in the
--   'YesodDB' monad so that state can be used / more requests can be made.
findGroupAncestor :: TcontentId -> (TgroupId -> YesodDB App (Maybe a)) -> YesodDB App (Maybe a)
findGroupAncestor bcid f = do
    seenRef <- newIORef (asSet mempty)
    let rec cid = do
            seen <- readIORef seenRef
            if cid `member` seen
                then return Nothing
                else selectSource [TmemberContent ==. cid] [] $$ loop
          where
            loop = await >>= maybe (return Nothing) go

            go (Entity _ Tmember {..}) = do
                c <- lift $ get404 tmemberHolder
                case c of
                    HolderTopLevelSum _ -> return Nothing
                    HolderGroupSum gid -> do
                        result <- lift $ f gid
                        result' <- case result of
                            Just x -> return (Just x)
                            Nothing -> do
                                mContent <- lift . getBy $ UniqueContentGroup gid
                                case mContent of
                                    Just (Entity cid' _) -> lift $ rec cid'
                                    Nothing -> return Nothing
                        case result' of
                            Just x -> return (Just x)
                            Nothing -> loop
    rec bcid

tutorialMarkdownSettings :: MarkdownSettings
tutorialMarkdownSettings = def
    { msStandaloneHtml = setFromList ["<hidden>", "</hidden>"]
    , msFencedHandlers = htmlFencedHandler "@@@" hiddenStart (const "</hidden>")
                      ++ msFencedHandlers def
    , msXssProtect = False -- has to be false, otherwise hidden and hoogle tags are removed
    }
  where
    hiddenStart t | null $ strip t = "<hidden>"
    hiddenStart t = concat
        [ "<hidden title=\""
        , concatMap escape t
        , "\">"
        ]
    escape '<' = "&lt;"
    escape '>' = "&gt;"
    escape '&' = "&amp;"
    escape '"' = "&quot;"
    escape c = singleton c

contentToHtml :: TutorialContent -> Html
contentToHtml (TutorialContent' orig)
    -- If this begins with a doctype statement, then treat as raw HTML
    | ["<!doctype"] == map toCaseFold (take 1 $ words orig) = preEscapedToMarkup orig
    | otherwise = markdown tutorialMarkdownSettings $ fromChunks [orig]

contentToNodes :: TutorialContent -> [Node]
contentToNodes content =
    nodes
  where
    Document _ (Element _ _ nodes) _ = toDoc content
    toDoc =
          parseLBS
        . Text.Blaze.Html.Renderer.Utf8.renderHtml
        . H.div
        . contentToHtml

-- | Determines if there are any active Haskell blocks within the content.
hasActiveHaskell :: TutorialContent -> Bool
hasActiveHaskell content =
    any hasActive nodes0
  where
    nodes0 = contentToNodes content
    hasActive (NodeElement (Element n as nodes)) = isActive n as || any hasActive nodes
    hasActive _ = False

    isActive "code" as =
        case lookup "class" as of
            Just (words -> cs) ->
              ("active" `elem` cs && "haskell" `elem` cs) ||
              "active-haskell" `elem` cs
            Nothing -> False
    isActive _ _ = False

renderTutorialContent :: TutorialContent
                      -> (Maybe Int -> Text -> Text) -- ^ create a Hoogle link
                      -> (Text -> Text) -- ^ wrap image URLs
                      -> Bool -- ^ XSS protection?
                      -> (Maybe Html, Html) -- ^ toc, content
renderTutorialContent tcontent createHoogleLink wrapImageUrl xssProtect =
    (toc', content)
  where
    toc' =
        case toc [] of
            [] -> Nothing
            toc'' -> Just $ tocHtml $ nestTocNodes toc''
    content = sanitize $ concat $ map toHtml nodes'

    sanitize
        | xssProtect = preEscapedToMarkup
                     . filterTags (balanceTags . fixTags . safeTags)
                     . toStrict
                     . renderHtml
        | otherwise = id

    fixTags (TagOpen "a" attrs:rest)
        | Just "youtube-video" <- lookup "class" attrs
        , Just videoId <- lookup "href" attrs
        , validVideoId videoId = TagOpen "iframe"
                [ ("width", "420")
                , ("height", "315")
                , ("src", "//www.youtube.com/embed/" ++ videoId)
                , ("frameborder", "0")
                , ("allowfullscreen", "allowfullscreen")
                ] : TagClose "iframe" : fixTags rest
    fixTags (x:xs) = x : fixTags xs
    fixTags [] = []

    validVideoId t =
        all safeChar t && not (null t)
      where
        -- Needed to ensure we don't allow some kind of XSS attack
        safeChar c =
            ('A' <= c && c <= 'Z') ||
            ('a' <= c && c <= 'z') ||
            ('0' <= c && c <= '9') ||
            c == '-' ||
            c == '_'

    nodes = contentToNodes tcontent

    (nodes', toc) = runState (mapM (goN 0) nodes) mempty

    goN _sections (NodeElement (Element "hoogle" as [NodeContent display])) =
        return $ NodeElement $ Element "a" (mapFromList
            [ ("href", createHoogleLink mresults search)
            , ("class", "hoogle")
            , ("title", title)
            ]) [NodeElement $ Element "code" mempty [NodeContent display]]
      where
        defTitle = "Hoogle search for: " ++ search
        title = fromMaybe defTitle $ lookup "title" as
        search = fromMaybe display $ lookup "search" as
        mresults = lookup "results" as >>= decimalMay
        decimalMay t =
            case decimal t of
                Right (i, "") -> Just i
                _ -> Nothing
    goN sections (NodeElement (Element "hidden" as cs)) = do
        cs' <- mapM (goN sections) cs
        return $ NodeElement $ Element "div" (insertMap "class" "hidden" as) cs'
    goN _sections (NodeElement (Element "youtube" _ [NodeContent videoId]))
        | validVideoId videoId =
            return $ NodeElement $ Element "a" (mapFromList
                [ ("class", "youtube-video")
                , ("href", videoId)
                ]) []

    goN sections (NodeElement (Element "img" as cs)) = do
        cs' <- mapM (goN sections) cs
        let as' =
                case lookup "src" as of
                    Nothing -> as
                    Just src -> insertMap "src" (wrapImageUrl src) as
        return $ NodeElement $ Element "img" as' cs'
    goN sections (NodeElement (Element n as cs))
        | Just level <- getHLevel sections n = do
            let title = toText cs
                slug =
                    case lookup "id" as of
                        Nothing -> slugify' title
                        Just i -> i
            tell (TocNode title slug level:)
            cs' <- mapM (goN sections) cs
            return $ NodeElement $ Element n (insertMap "id" slug as) (link slug cs')
        | otherwise = fmap (NodeElement . Element n as) $ mapM (goN sections') cs
      where
        sections'
            | n == "section" = sections + 1
            | otherwise = sections
        link slug inner = [NodeElement $ Element "a" (insertMap "href" ("#" <> slug) mempty) inner]
    goN _ n = return n

    tell x = modify (<> x)

    slugify' :: Text -> Text
    slugify' = omap toSafe . intercalate "-" . words . toLower

    toSafe c
        | 'a' <= c && c <= 'z' = c
        | '0' <= c && c <= '9' = c
        | otherwise = '-'

    getHLevel :: Int -- ^ section count
              -> Name
              -> Maybe Int
    getHLevel sections (Name local' Nothing Nothing) = do
        x <- stripPrefix "h" local'
        Right (i, "") <- Just $ decimal x
        guard $ i >= 1 && i <= 6
        Just $ i + sections
    getHLevel _ _ = Nothing

    toText =
        concat . map go
      where
        go (NodeElement (Element _ _ cs)) = toText cs
        go (NodeContent t) = t
        go (NodeInstruction _) = ""
        go (NodeComment _) = ""

    tocHtml [] = return ()
    tocHtml trees = H.ul $ mapM_ tocHtml' trees

    tocHtml' TocTree {..} = H.li $ do
        H.a H.! HA.href (H.toValue $ "#" ++ tocTreeId) $ toHtml tocTreeTitle
        tocHtml tocTreeChildren

data TocNode = TocNode
    { tocNodeTitle :: Text
    , tocNodeId    :: Text
    , tocNodeLevel :: Int
    }
    deriving (Show, Eq)

data TocTree = TocTree
    { tocTreeTitle    :: Text
    , tocTreeId       :: Text
    , tocTreeChildren :: [TocTree]
    }
    deriving (Show, Eq)

nestTocNodes :: [TocNode] -> [TocTree]
nestTocNodes [] = []
nestTocNodes (TocNode title id' level:rest) =
    TocTree title id' (nestTocNodes x) : nestTocNodes y
  where
    (x, y) = span (\t -> tocNodeLevel t > level) rest

-- | Same as 'tutorialVersionTitle', but will fill in a default value from the
-- content if no title is available.
tutorialVersionTitle' :: PublishedTutorial -> Title
tutorialVersionTitle' tv = defaultTitle (publishedTutorialTitle tv) (publishedTutorialContent tv)

-- | Provide a default title based on the content if title is blank.
defaultTitle :: Title -> TutorialContent -> Title
defaultTitle (Title title) (TutorialContent' content)
    | null title = Title $
        if null content
            then "<UNTITLED>"
            else take 100 content
    | otherwise = Title title

prettyTimeStamp :: UTCTime -> Text
prettyTimeStamp = pack . formatTime defaultTimeLocale "%e %b %Y, %l:%M%P"

-- | Get the URLs for the given content.
memberToURL :: (MonadResource m, MonadLogger m) => Tmember -> SqlPersistT m [Route App]
memberToURL = do
    continue (asSet mempty) []
  where
    go _ slugs (HolderTopLevelSum uid) = do
        mprofile <- getBy $ UniqueProfile uid
        case (mprofile, slugs) of
            (Just (Entity _ profile), x:xs) -> do
                let start
                        | profileHandle profile == UserHandle "school" = SchoolTutorialR
                        | otherwise = UserTutorialR $ profileHandle profile
                return [start x xs]
            _ -> return []
    go visited _ (HolderGroupSum gid) | gid `member` visited = return []
    go visited slugs (HolderGroupSum gid) = do
        let visited' = insertSet gid visited
        mcontent <- getBy $ UniqueContentGroup gid
        case mcontent of
            Nothing -> return []
            Just (Entity content _) -> selectSource [TmemberContent ==. content] []
                                    $$ concatMapM (continue visited' slugs . entityVal)
                                    =$ sinkList

    continue visited slugs member' = do
        mholder <- get $ tmemberHolder member'
        case mholder of
            Nothing -> return []
            Just holder -> go visited slugs' holder
      where
        slugs' = tmemberSlug member' : slugs

findOtherUsages :: TmemberId -- ^ ignored
                -> TcontentId
                -> YesodDB App ([TmemberId], [Text]) -- ^ owner links, foreign links
findOtherUsages currentMID cid = do
    content <- get404 cid
    mowner <-
        case content of
            TcontentGroupSum gid -> (Just . tgroupAuthor) <$> get404 gid
            TcontentTutorialSum tid -> (Just . tutorialAuthor) <$> get404 tid
            TcontentProjectSum _ -> return Nothing
    fmap partitionEithers
         $ selectSource [TmemberContent ==. cid, TmemberId !=. currentMID] []
        $$ awaitForever (toEither mowner)
        =$ sinkList
  where
    toEither Nothing _ = return ()
    toEither (Just owner) (Entity mid m) = do
        holder <- lift $ get404 $ tmemberHolder m
        owner' <-
            case holder of
                HolderTopLevelSum uid -> return uid
                HolderGroupSum gid -> lift $ tgroupAuthor <$> get404 gid
        if owner == owner'
            then yield $ Left mid
            else do
                render <- lift $ lift getUrlRender
                lift (memberToURL m) >>= mapM_ (yield . Right . render)

addMemberTo :: HolderId
            -> Tcontent
            -> TutorialName
            -> YesodDB App ()
addMemberTo holderid content slug = do
    updateWhere [TmemberHolder ==. holderid] [TmemberPriority +=. 1]
    contentid <- insert content
    insert_ Tmember
        { tmemberContent = contentid
        , tmemberHolder = holderid
        , tmemberSlug = slug
        , tmemberSlugUserGen = False
        , tmemberPriority = 0
        }

-- | Same as 'tutorialTitle', but will fill in a default value from the
-- content if no title is available.
tutorialTitle' :: Tutorial -> Title
tutorialTitle' t = defaultTitle (tutorialTitle t) (tutorialContent t)

-- | Get the header, footer, and sidebar for a tutorial. See:
getTutorialExtras :: Handler (Html, Html, Html)
getTutorialExtras = return (return (), return (), return ())

getContentId :: (key -> Unique Tcontent) -> (key -> Tcontent) -> key -> Handler TcontentId
getContentId uniq constr key = $runDB $ do
    mtc <- getBy $ uniq key
    case mtc of
        Just (Entity tcid _) -> return tcid
        Nothing -> insert $ constr key

getTutorialContentId :: TutorialId -> Handler TcontentId
getTutorialContentId = getContentId UniqueContentTutorial TcontentTutorialSum

getGroupContentId :: TgroupId -> Handler TcontentId
getGroupContentId = getContentId UniqueContentGroup TcontentGroupSum
