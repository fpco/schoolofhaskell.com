{-# OPTIONS_GHC -fno-warn-orphans #-}
module Import (module Import) where

import           ClassyPrelude.Yesod as Import hiding (fileName, runDB)
import           Data.Text.Lazy (strip)
import qualified Data.UUID as U
import           Data.UUID.V4 (nextRandom)
import qualified FP.Store.Blob as Blob
import           Foundation as Import
import           Import.Files as Import
import           Import.Formatting as Import
import           Import.Prune as Import
import           Import.Recent as Import
import           Import.Tutorial as Import
import           Import.User as Import
import           Settings.Development as Import
import           Settings.StaticFiles as Import
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.HTML.TagStream.Text (tokenStream, Token)
import           Text.HTML.TagStream.Types (Token' (Text, TagOpen))
import           Yesod.Form.Jquery as Import (urlJqueryJs)

newMediaBlobUUID :: MonadIO m => m Blob.MediaUUID
newMediaBlobUUID = liftM (Blob.MediaUUID . pack . U.toString) $ liftIO nextRandom

displayTutorial :: TutorialContent
                -> Handler (Maybe Widget, Widget) -- ^ aside, main body
displayTutorial = displayTutorial' True

displayBlog :: Either Html TutorialContent -> Widget
displayBlog = either
    toWidget
    (join . liftHandlerT . fmap snd . displayTutorial' False)

getWrapImageUrl :: Handler (Text -> Text)
getWrapImageUrl = do
    render <- getUrlRenderParams
    return $ \src -> render ContentProxyR [("src", src)]

displayTutorial' :: Bool -- ^ XSS protection
                 -> TutorialContent
                 -> Handler (Maybe Widget, Widget) -- ^ aside, main body
displayTutorial' xssProtect content = do
    (toc, body) <- tutorialTocAndBody xssProtect content
    let aside = [whamlet|
                    $maybe toc' <- toc
                        <h3>Sections
                        \#{toc'}
                |]
        body' = [whamlet|<article>#{body}|]
    return (Just aside, body')

tutorialTocAndBody:: Bool -- ^ XSS protection
                  -> TutorialContent
                  -> Handler (Maybe Html, Html) -- ^ aside, main body
tutorialTocAndBody xssProtect content = do
    let mkHoogle mcnt query = "https://www.stackage.org/lts/hoogle?q=" <> query <> maybe "" (\cnt -> "&results=" <> tshow cnt) mcnt
    wrapImageUrl <- getWrapImageUrl
    return $ renderTutorialContent content mkHoogle wrapImageUrl xssProtect

prettyDay :: UTCTime -> Text
prettyDay = pack . formatTime defaultTimeLocale "%e %b %Y"

prettyUserId :: UserId -> YesodDB App Text
prettyUserId uid = do
    mp <- getBy $ UniqueProfile uid
    case mp of
        Nothing -> return $ "User #" ++ toPathPiece uid
        Just (Entity _ p) -> return $ prettyProfile p

instance ToBuilder Token TextBuilder where
    toBuilder (Text s) = toBuilder s
    toBuilder (TagOpen s _ _) | s `member` blocks = toBuilder ' '
      where
        blocks = asHashSet $ setFromList
            [ "article"
            , "aside"
            , "blockquote"
            , "body"
            , "br"
            , "button"
            , "canvas"
            , "caption"
            , "col"
            , "colgroup"
            , "dd"
            , "div"
            , "dl"
            , "dt"
            , "embed"
            , "fieldset"
            , "figcaption"
            , "figure"
            , "footer"
            , "form"
            , "h1"
            , "h2"
            , "h3"
            , "h4"
            , "h5"
            , "h6"
            , "header"
            , "hgroup"
            , "hr"
            , "li"
            , "map"
            , "object"
            , "ol"
            , "output"
            , "p"
            , "pre"
            , "progress"
            , "section"
            , "table"
            , "tbody"
            , "textarea"
            , "tfoot"
            , "th"
            , "thead"
            , "tr"
            , "ul"
            , "video"
            ]
    toBuilder _ = mempty

-- | Extract plain text from a body of @Html@.
plainText :: Html -> LText
plainText html = strip
               $ runIdentity
               $ mapM_ yield (toChunks $ renderHtml html)
              $$ tokenStream
              =$ sinkLazyBuilder

-- | Ellipsize the given text up to the given length.
ellipsize :: Int64 -> LText -> LText
ellipsize l t
    | olength64 t <= l = t
    | otherwise = loop 0 id $ words t
  where
    loop _ front [] = unwords $ front []
    loop total front (x:xs)
        | olength64 x + total + 2 > l = concat $ front ["\x2026"]
        | otherwise = loop
            (total + olength64 x + 1)
            (front . addSpace . (x:))
            xs
      where
        addSpace
            | total == 0 = id
            | otherwise = (" ":)

-- | A deconstructor form results.
formResult :: t -> (t1 -> t) -> FormResult t1 -> t
formResult _ k (FormSuccess v) = k v
formResult nil _ _             = nil

titleField :: Field Handler Title
titleField = checkMMap (return . checkTitle) unTitle textField

checkTitle :: Text -> Either Text Title
checkTitle title
    | title == "" = Right $ Title "Untitled"
    | length title < 512 = Right $ Title title
    | otherwise = Left "The specified title is too long."
