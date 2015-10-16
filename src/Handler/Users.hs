module Handler.Users where

import Import

getUsersR :: Handler Html
getUsersR = do
    users <- $runDB $ selectList [UserSummaryTutcount >. 0] [Desc UserSummaryTutcount]
    defaultLayout $ do
        setTitle "Users"
        $(widgetFile "users")
