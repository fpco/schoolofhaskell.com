module Handler.Export
  ( getExportR
  , performExportLoop
  ) where

import Import hiding ((</>), FilePath, member, (<.>))
import System.IO.Temp
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Process
import System.Exit (ExitCode (..))
import System.FilePath ((</>), FilePath, (<.>))
import qualified Data.ByteString as B
import Control.Concurrent (threadDelay)
import Data.Conduit.Pool (Pool)

getExportR :: Handler Text
getExportR = do
  ref <- appExportStatus <$> getYesod
  status <- readIORef ref
  case status of
    ExportNotComplete -> return "Export is not yet complete"
    ExportError e -> error e
    ExportSuccess fp -> sendFile "application/x-tar" fp

performExportLoop :: IORef ExportStatus -> Pool (ignored, SqlBackend) -> IO ()
performExportLoop ref pool = forever $ do
  eres <- tryAny $ runSqlPool pool performExport
  join $ atomicModifyIORef' ref $ \old ->
    let new = either (ExportError . show) ExportSuccess eres
        action =
          case old of
            ExportSuccess oldfp -> void $ tryAny $ removeFile oldfp
            _ -> return ()
     in (new, action)
  threadDelay $ 1000 * 1000 * 60 * 60 * 12

performExport :: ReaderT SqlBackend IO FilePath
performExport = withSystemTempDirectory "export" $ \dir' -> do
  (tarfp, tarh) <- liftIO $ openTempFile "/tmp" "export.tar"
  liftIO $ hClose tarh
  let dir = dir' </> "export"
  profiles :: [Entity Profile] <- selectList [] []
  forM_ profiles $ \(Entity _ profile) -> do
    let (UserHandle h) = profileHandle profile
    mholder <- getBy $ UniqueHolderTopLevel $ profileUser profile
    forM_ mholder $ handleHolder (dir </> unpack h)
  (_, _, _, ph) <- liftIO $ createProcess (proc "tar" ["cf", tarfp, "export"])
    { cwd = Just dir'
    }
  ec <- liftIO $ waitForProcess ph
  case ec of
    ExitSuccess -> return tarfp
    _ -> error $ "Error calling tar: " ++ show ec

handleHolder :: FilePath -> Entity Holder -> ReaderT SqlBackend IO ()
handleHolder dir (Entity hid _) = do
  members <- selectList [TmemberHolder ==. hid] []
  forM_ members $ \(Entity _ member) -> do
    mcontent <- get $ tmemberContent member
    case mcontent of
      Nothing -> return ()
      Just (TcontentTutorialSum tutid) -> do
        mtut <- getBy $ UniquePublishedTutorial tutid
        forM_ mtut $ handleTutorial dir (tmemberSlug member)
      Just (TcontentGroupSum gid) -> do
        mholder <- getBy $ UniqueHolderGroup gid
        forM_ mholder $ handleHolder (dir </> toPath (tmemberSlug member))
      Just (TcontentProjectSum _) -> return ()

toPath :: TutorialName -> FilePath
toPath (TutorialName name') = map fixC $ unpack name'
  where
    fixC ':' = '_'
    fixC '/' = '_'
    fixC c = c

handleTutorial :: FilePath -> TutorialName -> Entity PublishedTutorial -> ReaderT SqlBackend IO ()
handleTutorial dir name' (Entity _ tut) = do
  let name = toPath name'
  liftIO $ createDirectoryIfMissing True dir
  let (TutorialContent' content) = publishedTutorialContent tut
  let fp = dir </> name <.> "md"
  liftIO $ B.writeFile fp $
    encodeUtf8 $ "# " <>
    unTitle (publishedTutorialTitle tut) <>
    "\n\n" <> content
