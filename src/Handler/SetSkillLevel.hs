module Handler.SetSkillLevel where

import Import
import qualified Network.Wai as WAI

skillLevelForm :: Maybe SkillLevel -> Form (Maybe SkillLevel)
skillLevelForm sl = renderDivs $
    aopt (selectField optionsEnum) "Skill level" (Just sl)

postSetSkillLevelR :: TutorialId -> Handler ()
postSetSkillLevelR tid = do
    Entity _ user <- requireAuth
    app <- getYesod
    ia <- isAdmin app user
    unless ia $ permissionDenied "Page requires admin access"
    Tutorial {..} <- $runDB $ get404 tid
    ((res, _), _) <- runFormPost $ skillLevelForm tutorialSkill
    case res of
        FormSuccess msl -> do
            $runDB $ update tid [TutorialSkill =. msl]
            setMessage "Skill level updated"
        _ -> setMessage "Invalid submission"
    req <- waiRequest
    case lookup "referer" $ WAI.requestHeaders req of
        Nothing -> redirect HomeR
        Just t -> redirect $ decodeUtf8 t
