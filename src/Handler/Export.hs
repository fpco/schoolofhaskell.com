module Handler.Export
  ( getExportR
  ) where

import Import hiding ((</>), FilePath, member, (<.>))
import System.IO.Temp
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Process
import Control.Monad.Trans.Resource (allocate)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), FilePath, (<.>))
import qualified Data.ByteString as B

getExportR :: Handler ()
getExportR = do
  (_key, (tarfp, tarh)) <- allocate (openTempFile "/tmp" "export.tar") (removeFile . fst)
  liftIO $ hClose tarh
  withSystemTempDirectory "export" $ \dir' -> do
    let dir = dir' </> "export"
    $runDB $ do
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
      ExitSuccess -> sendFile "application/x-tar" tarfp
      _ -> error $ "Error calling tar: " ++ show ec

handleHolder :: FilePath -> Entity Holder -> ReaderT SqlBackend Handler ()
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

handleTutorial :: FilePath -> TutorialName -> Entity PublishedTutorial -> ReaderT SqlBackend Handler ()
handleTutorial dir name' (Entity _ tut) = do
  let name = toPath name'
  liftIO $ createDirectoryIfMissing True dir
  let (TutorialContent' content) = publishedTutorialContent tut
  let fp = dir </> name <.> "md"
  liftIO $ B.writeFile fp $
    encodeUtf8 $ "# " <>
    unTitle (publishedTutorialTitle tut) <>
    "\n\n" <> content
