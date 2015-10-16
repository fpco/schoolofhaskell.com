module Handler.ExportAllTutorials where

import           Blaze.ByteString.Builder (Builder)
import           Blaze.ByteString.Builder.ByteString (fromByteString)
import           Control.Monad.Trans.Resource (allocate)
import qualified Data.Conduit.List as CL
import           Filesystem (listDirectory, createTree, removeTree, openFile, IOMode(..))
import           Filesystem.Path (parent)
import           Import hiding (Builder)
import           System.Exit (ExitCode(..))
import           System.IO.Temp (createTempDirectory)
import           System.Process (createProcess, proc, CreateProcess(..), waitForProcess)

getExportAllTutorialsR :: Handler TypedContent
getExportAllTutorialsR = do
    uid <- requireAuthId
    Entity hid _ <- $runDB $ getTopHolder uid
    (_, dirString) <- allocate (createTempDirectory "/tmp" "export-all-tutorials")
                               (removeTree . fpFromString)
    let dir = fpFromString dirString
    -- Write tutorials to the temporary directory.
    recursiveTutorialsSource uid hid Nothing $=
        CL.map tutorialToFile $$
        sinkDirectory dir
    files <- liftIO $ map (fpToString . filename) <$> listDirectory dir
    when (null files) notFound
    -- Make a .tar.gz of all the tutorials.
    let tarball = dir </> "export-all-tutorials.tar.gz"
    (Nothing, Nothing, Nothing, ph) <- liftIO $ createProcess
            (proc "tar" ("czf" : fpToString tarball : files))
                { cwd = Just (fpToString dir)
                }
    ec <- liftIO $ waitForProcess ph
    if ec == ExitSuccess then return () else throwM ec
    -- Serve the resulting tarball.
    (_, h) <- allocate (openFile tarball ReadMode) hClose
    addHeader "Content-Disposition" "attachment; filename=all-tutorials.tar.gz"
    respondSource "application/x-tar" (sourceHandle h $= flushEveryChunk)

tutorialToFile :: (TutorialPath, Entity Tutorial) -> (FilePath, ByteString)
tutorialToFile (tp, Entity _ tut) = (fpFromText tp, encodeUtf8 contents)
  where
    desc = unTextarea (tutorialDesc tut)
    contents =
        "title: " <> unTitle (tutorialTitle tut) <> "\n" <>
        (if null desc
             then ""
             else "description: \n" <> indentLines desc) <> "\n" <>
        (unTutorialContent (tutorialContent tut))
    indentLines = unlines . map (indent 4) . lines
    indent n xs = replicate n ' ' <> xs

type TutorialPath = Text

recursiveTutorialsSource :: UserId -> HolderId -> Maybe TutorialPath -> Source Handler (TutorialPath, Entity Tutorial)
recursiveTutorialsSource uid hid mtp = do
    ms <- lift $ $runDB $ selectList [TmemberHolder ==. hid] []
    forM_ ms $ \(Entity _ m) -> do
        let tp = maybe "" (<> "/") mtp <> toPathPiece (tmemberSlug m)
        content <- lift $ $runDB $ get404 $ tmemberContent m
        case content of
            TcontentGroupSum gid -> do
                Entity hid' _ <- lift $ $runDB $ getBy404 (UniqueHolderGroup gid)
                recursiveTutorialsSource uid hid' (Just tp)
            TcontentTutorialSum tid -> do
                tut <- lift $ $runDB $ get404 tid
                let tutEnt = Entity tid tut
                -- Make sure that either the user is the author, or
                -- the tutorial is public.  For non SoH accounts, I
                -- don't think this is really necessary.  But I think
                -- it's better to check!
                if tutorialAuthor tut == uid
                    then yield (tp, tutEnt)
                    else do
                        mpublished <- lift $ $runDB $ getPublishedTutorial tutEnt
                        forM_ mpublished $ \_ -> yield (tp, tutEnt)
            TcontentProjectSum _ -> return ()

-- NOTE: Doesn't guarantee that files will only be written into the
-- directory.
--
-- I believe that it isn't possible to have "../", etc as group
-- names. Maybe this should be made 100% certain of by protecting from
-- this here?
sinkDirectory :: MonadIO m => FilePath -> Consumer (FilePath, ByteString) m ()
sinkDirectory dir = mapM_C $ \(relPath, x) -> liftIO $ do
    let path = dir </> relPath
    createTree (parent path)
    writeFile path x

flushEveryChunk :: Monad m => Conduit ByteString m (Flush Builder)
flushEveryChunk =
    CL.concatMap (\bs -> [Chunk $ fromByteString bs, Flush])
