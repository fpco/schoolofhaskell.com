-- | Create a new group.

module Handler.NewGroup where

import Import
import Import.NewContentMember

-- | New group route.
getNewGroupR :: TutorialNames -> Handler Html
getNewGroupR tns = do
    Entity _ profile <- requireProfile
    getNewHelper profile $ NewHelper form TcontentGroupSum tgroupTitle "group" UserTutorialR tns

-- | New group post route.
postNewGroupR :: TutorialNames -> Handler Html
postNewGroupR = getNewGroupR

-- | New group form.
form :: UserId -> Form Tgroup
form uid = renderTable $ Tgroup
    <$> pure uid
    <*> areq titleField "Title" Nothing
    <*> (fromMaybe (Textarea "") <$> aopt textareaField "Description" Nothing)
    <*> lift (liftIO getCurrentTime)
