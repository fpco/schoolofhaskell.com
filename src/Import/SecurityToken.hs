module Import.SecurityToken where

import ClassyPrelude.Yesod hiding (runDB)
import Foundation

getSecurityToken :: Handler SecurityToken
getSecurityToken = do
    Entity _ u <- requireAuth
    return $ userSecurityToken u

resetSecurityToken :: UserId -> Handler SecurityToken
resetSecurityToken uid = $runDB $ do
    s <- newSecurityToken
    update uid [UserSecurityToken =. s]
    return s

profileByHandle :: UserHandle -> YesodDB App (Entity Profile)

-- MS 2013-04-21 The following should be sufficient, but I got segfaults when
-- using it. The segfaults seem to be a bug in GHC, and one I've encountered
-- before. I'm not sure if it's simply a matter of a badly compiled library,
-- but I want to make sure the bug never rears its head in production.
-- Therefore, writing this the long way instead.
--
--profileByHandle = either (const $ lift notFound) (getBy404 . UniqueNormalizedHandle) . normalizeHandle

profileByHandle uh =
    case normalizeHandle uh of
        Left _ -> lift notFound
        Right nh -> do
            mp <- getBy $ UniqueNormalizedHandle nh
            case mp of
                Nothing -> notFound
                Just p -> return p
