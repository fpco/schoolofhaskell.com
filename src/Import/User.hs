module Import.User where

import ClassyPrelude.Yesod
import Foundation
import qualified Data.Text as T
import Network.Gravatar (gravatar, gSize, Size (Size))
import qualified Data.Conduit.List as CL
import System.Directory (removeFile)
import System.IO (openTempFile)
import System.Process (rawSystem)
import System.Exit (ExitCode (ExitSuccess))

populateUserSummary :: (MonadLogger m, MonadResource m) => UserId -> SqlPersistT m ()
populateUserSummary uid = do
    mu <- get uid
    mp <- getBy $ UniqueProfile uid
    case (,) <$> mu <*> mp of
        Nothing -> return ()
        Just (u, Entity _ p) -> do
            tutcount <- selectKeys [TutorialAuthor ==. uid] []
                     $$ CL.mapM toTutCount
                     =$ CL.fold (+) 0
            let us = UserSummary
                    { userSummaryHandle = profileHandle p
                    , userSummaryDisplay = prettyProfile p
                    , userSummaryGravatar = userGravatar 120 u
                    , userSummaryTutcount = tutcount
                    }
            deleteWhere [UserSummaryHandle ==. profileHandle p]
            insert_ us
  where
    toTutCount tid = do
        mpt <- getBy $ UniquePublishedTutorial tid
        if isJust mpt
            then do
                mc <- getBy $ UniqueContentTutorial tid
                case mc of
                    Nothing -> return 0
                    Just (Entity cid _) -> do
                        members <- count [TmemberContent ==. cid]
                        return $ if members > 0 then 1 else 0
            else return 0

userGravatar :: Int -- ^ width/height
             -> User
             -> Text
userGravatar size = T.replace "http://" "https://" . pack . gravatar def { gSize = Just $ Size size } . userEmail

getSSHKeyPair :: Entity Profile -> YesodDB App SSHKeyPair
getSSHKeyPair (Entity pid p) =
    case profileSshKeyPair p of
        Just x -> return x
        Nothing -> do
            x <- genKeyPair
            update pid [ProfileSshKeyPair =. Just x]
            return x
  where
    genKeyPair = do
        (privateFP, tmpHandle) <- liftIO $ openTempFile "/tmp" "keygen"
        let publicFP = privateFP ++ ".pub"
        liftIO $ hClose tmpHandle
        liftIO $ removeFile privateFP
        ec <- liftIO $ rawSystem "ssh-keygen"
            [ "-t"
            , "rsa"
            , "-N"
            , ""
            , "-f"
            , privateFP
            , "-C"
            , "School of Haskell"
            ]
        if ec == ExitSuccess
            then liftIO $ do
                private <- readFile $ fpFromString privateFP
                removeFile privateFP
                public <- readFile $ fpFromString publicFP
                removeFile publicFP
                return $ SSHKeyPair
                    { publicKey = public
                    , privateKey = private
                    }
            else do
                $logError $ "Could not generate keypair, exit code: " ++ tshow ec ++ " for user " ++ tshow pid
                error "I'm sorry, we had a problem generating your key pair"

lookupUserNameOrEmail :: Text -> YesodDB App (Either Text UserId)
lookupUserNameOrEmail ident = do
    mi <- getUserByEmail False ident
    case mi of
        Just (Entity uid _) -> return $ Right uid
        Nothing ->
            case normalizeHandle $ UserHandle ident of
                Left _ -> return $ Left "Username / email not found"
                Right nh -> do
                    mp <- getBy $ UniqueNormalizedHandle nh
                    case mp of
                        Nothing -> return $ Left "Username / email not found"
                        Just (Entity _ Profile {..}) -> return $ Right profileUser
