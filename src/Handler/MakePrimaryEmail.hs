module Handler.MakePrimaryEmail where

import Import
import Yesod.Auth.Email (isValidPass)

postMakePrimaryEmailR :: IdentId -> Handler Html
postMakePrimaryEmailR iid = do
    Entity uid user <- requireAuth
    salted <-
        case userPassword user of
            Nothing -> do
                setMessage "In order to change the primary email address, you must first set a password"
                redirect ProfileR
            Just salted -> return salted
    ((mres, widget), enctype) <- runFormPost $ renderBootstrap2 $ areq (check (checkPass salted) passwordField) "Password" Nothing
    case mres of
        FormSuccess _password -> do
            $runDB $ do
                Ident {..} <- get404 iid
                unless identEmail $ lift notFound
                unless (identUser == uid) $ lift notFound
                update uid [UserEmail =. identIdent]
                lift $ setMessage $ toHtml $ "Primary email address is now " ++ identIdent
            redirect ProfileR
        _ -> defaultLayout $ do
            setTitle "Confirm primary address change"
            [whamlet|
                <p>In order to change your primary email address, please enter your password.
                <form method=post enctype=#{enctype}>
                    ^{widget}
                    <div>
                        <input .btn type=submit value="Confirm">
                        <a href=@{ProfileR} .btn>Cancel
            |]
  where
    checkPass salted password
        | isValidPass password salted = Right password
        | otherwise = Left $ asText "Password is incorrect"
