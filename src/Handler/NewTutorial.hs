-- | Create a new tutorial.

module Handler.NewTutorial where

import           Import
import           Import.NewContentMember

getNewTutorialR :: TutorialNames -> Handler Html
getNewTutorialR = redirectToPost . NewTutorialR

-- | New tutorial post route.
postNewTutorialR :: TutorialNames -> Handler Html
postNewTutorialR tns = do
    uid <- requireAuthId
    Entity _ profile <- requireProfile
    createNewHelper
        profile
        TcontentTutorialSum
        tutorialTitle
        Nothing
        (const EditTutorialR)
        tns
        (return ())
        Tutorial
            { tutorialAuthor = uid
            , tutorialTitle = Title "Untitled"
            , tutorialDesc = Textarea ""
            , tutorialContent = TutorialContent' ""
            , tutorialConcurrentToken = def
            , tutorialIsDirty = False
            , tutorialSkill = Nothing
            , tutorialPreviewCode = Nothing
            , tutorialEnvironment = Nothing
            }
