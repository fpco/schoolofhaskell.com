module Handler.AdminSystem where

import Import
{-
import Shelly (shelly, silently)
import Data.Text (strip)
import FP.Aws (awsRuby)
import FP.EnvSettings
-}

getAdminSystemR :: Handler Html
getAdminSystemR = do
    defaultLayout [whamlet| FIXME |]

{-
    maybeLoading <- lookupSession loadingSessionKey
    SystemInfo{..} <- case maybeLoading of
        Just _ -> do
            deleteSession loadingSessionKey
            getSystemInfo
        Nothing -> do
            render <- getUrlRender
            setSession loadingSessionKey $ pack $ show True
            setMessage "Please wait while retrieving system information..."
            addHeader "Refresh" $ "1;" ++ render AdminSystemR
            return $ SystemInfo Nothing Nothing Nothing []
    defaultLayout $ do
        setTitle "Admin System Information"
        $(widgetFile "admin-system")
  where
    getSystemInfo = do
        mStack' <- case awsInstanceID of
            Nothing -> return Nothing
            Just instanceId -> do
                sn <- strip <$> shrb "puts ec2.instances[ARGV[0]].tags['aws:cloudformation:stack-name']"
                                     [instanceId]
                return $ if null sn then Nothing else Just sn
        asgs' <- case mStack' of
            Nothing -> return []
            Just stack -> do
                asgNames <- lines <$> shrb
                    (unlines [ "cfm.stacks[ARGV[0]].resources.each {|r|"
                             , "    if r.resource_type == 'AWS::AutoScaling::AutoScalingGroup' then"
                             , "        puts r.physical_resource_id"
                             , "    end"
                             , "}" ])
                    [stack]
                forM asgNames $ \asgName -> do
                    asgInstances <- map words . lines <$> shrb
                        (unlines [ "as.groups[ARGV[0]].ec2_instances.each {|i|"
                                 , "    if i.status == :running then"
                                 , "        puts \"#{i.instance_id} #{i.public_dns_name} "
                                              ++ "#{i.private_ip_address} #{i.availability_zone}\""
                                 , "    end"
                                 , "}"])
                        [asgName]
                    siaInstances <- forM asgInstances $ \(instanceId : instanceDns :
                                                         instancePrivateIp : instanceZone : _) -> do
                        return $ SiaInstance instanceId instanceDns instancePrivateIp
                                             instanceZone
                    return $ SiAsg asgName siaInstances
        return $ SystemInfo fpEnvironmentNameMaybe awsInstanceID mStack' asgs'
    loadingSessionKey = "Handler.AdminSystem.loading"

shrb :: Text -> [Text] -> Handler Text
shrb r a = liftIO $ shelly $ silently $ awsRuby r a

data SystemInfo = SystemInfo
    { siEnvironment     :: Maybe Text
    , siInstanceId      :: Maybe Text
    , siStack           :: Maybe Text
    , siAsgs            :: [SiAsg] }

data SiAsg = SiAsg
    { siaName           :: Text
    , siaInstances      :: [SiaInstance] }

data SiaInstance = SiaInstance
    { siiInstanceId :: Text
    , siiDnsName    :: Text
    , siiPrivateIp  :: Text
    , siiZone       :: Text }
-}
