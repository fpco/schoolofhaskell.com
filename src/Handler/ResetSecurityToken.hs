module Handler.ResetSecurityToken where

import Import

postResetSecurityTokenR :: Handler ()
postResetSecurityTokenR = do
    Entity uid _ <- requireAuth
    void $ resetSecurityToken uid
    setMessage "Security token reset"
    redirect ProfileR
