module Handler.NewSshKey where

import Import

postNewSshKeyR :: Handler ()
postNewSshKeyR = do
    Entity pid p <- requireProfile
    $runDB $ update pid [ProfileSshKeyPair =. Nothing]
    case profileGithubAccessKey p of
        Nothing -> return ()
        Just key -> deleteGithubSsh key (Entity pid p)
    setMessage "SSH key reset"
    redirect ProfileR
